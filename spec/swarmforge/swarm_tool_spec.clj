(ns swarmforge.swarm-tool-spec
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [speclj.core :refer :all]))

(when-not (find-ns 'swarm-tool)
  (load-file "swarmforge/scripts/swarm_tool.bb"))

(def pom-with-parent
  (str "<project>\n"
       "  <modelVersion>4.0.0</modelVersion>\n"
       "  <parent>\n"
       "    <groupId>com.unclebob</groupId>\n"
       "    <artifactId>unclebob-parent</artifactId>\n"
       "    <version>1.0</version>\n"
       "  </parent>\n"
       "  <artifactId>mutate4java</artifactId>\n"
       "</project>\n"))

(defn temp-dir []
  (str (fs/create-temp-dir)))

(defn write-pom! [dir content]
  (spit (str (fs/path dir "pom.xml")) content)
  dir)

(defn read-pom [dir]
  (slurp (str (fs/path dir "pom.xml"))))

(defn exit-thrower [status message]
  (throw (ex-info "exit" {:status status :message message})))

(defn exit-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(describe "patch-mutate4java-pom!"
  (it "removes the parent block"
    (let [dir (write-pom! (temp-dir) pom-with-parent)]
      (swarm-tool/patch-mutate4java-pom! dir)
      (should-not-contain "<parent>" (read-pom dir))
      (should-not-contain "unclebob-parent" (read-pom dir))))

  (it "inlines the groupId and version the parent used to supply"
    (let [dir (write-pom! (temp-dir) pom-with-parent)]
      (swarm-tool/patch-mutate4java-pom! dir)
      (should-contain (str "<groupId>com.unclebob</groupId>\n"
                           "  <version>0.1.0-SNAPSHOT</version>\n"
                           "  <artifactId>mutate4java</artifactId>")
                      (read-pom dir))))

  (it "leaves a pom without a parent untouched"
    (let [content "<project>\n  <artifactId>mutate4java</artifactId>\n</project>\n"
          dir (write-pom! (temp-dir) content)]
      (swarm-tool/patch-mutate4java-pom! dir)
      (should= content (read-pom dir)))))

(describe "build-mutate4java!"
  (it "runs mvn -DskipTests package in the source dir"
    (let [calls (atom [])]
      (with-redefs [sh/sh (fn [& args] (swap! calls conj args) {:exit 0 :out "" :err ""})]
        (swarm-tool/build-mutate4java! "/src/mutate4java"))
      (should= [["mvn" "-DskipTests" "package" :dir "/src/mutate4java"]] @calls)))

  (it "exits 1 with the Maven output when the build fails"
    (with-redefs [sh/sh (fn [& _] {:exit 1 :out "BUILD FAILURE" :err "boom"})
                  swarm-tool/exit! exit-thrower]
      (let [{:keys [status message]} (exit-data #(swarm-tool/build-mutate4java! "/src"))]
        (should= 1 status)
        (should-contain "Failed to build mutate4java" message)
        (should-contain "boom" message)
        (should-contain "BUILD FAILURE" message)))))

(describe "write-mutate4java-wrapper!"
  (it "writes an executable wrapper that runs the built jar from the source dir"
    (let [root (temp-dir)
          path (swarm-tool/write-mutate4java-wrapper! root "/src/mutate4java")
          body (slurp (str path))]
      (should= (str (fs/path root ".swarmforge" "bin" "mutate4java")) (str path))
      (should (fs/executable? path))
      (should-contain "cd '/src/mutate4java'\n" body)
      (should-contain "exec java -jar target/mutate4java-0.1.0-SNAPSHOT.jar \"$@\"\n" body)))

  (it "forces the mutation worker limit"
    (let [body (slurp (str (swarm-tool/write-mutate4java-wrapper! (temp-dir) "/src")))]
      (should-contain "--max-workers 4" body)
      (should-contain "--mutate-all) shift" body))))

(describe "install-maven-build!"
  (it "patches the pom, builds with Maven, and writes the jar wrapper"
    (let [root (temp-dir)
          src (write-pom! (temp-dir) pom-with-parent)
          calls (atom [])]
      (with-redefs [swarm-tool/source-dir (fn [_ _] (fs/path src))
                    sh/sh (fn [& args] (swap! calls conj args) {:exit 0 :out "" :err ""})]
        (let [path (swarm-tool/install-maven-build! root {:source "github.com/unclebob/mutate4java"})]
          (should-not-contain "<parent>" (read-pom src))
          (should= [["mvn" "-DskipTests" "package" :dir src]] @calls)
          (should-contain (str "cd '" src "'") (slurp (str path)))))))

  (it "clones the source when it has no pom.xml"
    (let [root (temp-dir)
          src (str (fs/path (temp-dir) "mutate4java"))
          clones (atom [])]
      (with-redefs [swarm-tool/source-dir (fn [_ _] (fs/path src))
                    swarm-tool/clone-source! (fn [dir source]
                                               (swap! clones conj [(str dir) source])
                                               (fs/create-dirs dir)
                                               (write-pom! dir pom-with-parent))
                    sh/sh (fn [& _] {:exit 0 :out "" :err ""})]
        (swarm-tool/install-maven-build! root {:source "github.com/unclebob/mutate4java"})
        (should= [[src "github.com/unclebob/mutate4java"]] @clones)))))

(describe "install-one! for mutate4java"
  (it "takes the Maven build path instead of writing a bb-task wrapper"
    (let [root (temp-dir)
          src (write-pom! (temp-dir) pom-with-parent)]
      (with-redefs [swarm-tool/project-root (fn [] root)
                    swarm-tool/source-dir (fn [_ _] (fs/path src))
                    sh/sh (fn [& _] {:exit 0 :out "" :err ""})]
        (with-out-str (swarm-tool/install-one! "mutate4java"))
        (let [body (slurp (str (fs/path root ".swarmforge" "bin" "mutate4java")))]
          (should-contain "exec java -jar" body)
          (should-not (str/includes? body "exec bb")))))))
