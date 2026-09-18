# TODO

- [x] A role configured with backend `deepseek` produces a launch command that invokes the `codex` CLI with the DeepSeek model profile (`--profile deepseek`), so any pack role can be assigned to a DeepSeek reasoning agent.
- [x] A swarmforge.conf role configured with backend `deepseek` passes config validation instead of being rejected with "Unsupported agent", since `known-agents` was never updated to include it.
- [x] check-backend-dependencies! checks for the `codex` binary (not a nonexistent `deepseek` binary) when a role's backend is `deepseek`, so startup doesn't fail with "'deepseek' is required but not installed" even though codex is present.
