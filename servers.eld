;; Language Server Configuration for lsp-installer
;; This file contains server definitions with unified configuration format

;; Unified format for all servers:
;; :name - Server identifier
;; :description - Human readable description
;; :install-method - Installation method: "npm", "pip", "go", "gem", "github", "binary", "dotnet"
;; :source - Package name, GitHub repo, or URL depending on install method
;; :executable - Executable name/path (always required)
;; :options - Method-specific options (optional)
;; :path-dirs - List of directories to add to exec-path (relative to server install dir, required)

((:name
  "typescript-language-server"
  :description "TypeScript & JavaScript language server"
  :install-method "npm"
  :source "typescript-language-server"
  :executable "typescript-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "pyright"
  :description "Python language server"
  :install-method "npm"
  :source "pyright"
  :executable "pyright"
  :path-dirs ("node_modules/.bin"))

 (:name
  "pylsp"
  :description "Python LSP Server"
  :install-method "pip"
  :source "python-lsp-server"
  :executable "pylsp"
  :path-dirs ("venv/Scripts" "venv/bin"))

 (:name
  "rust-analyzer"
  :description "Rust language server"
  :install-method "github"
  :source "rust-lang/rust-analyzer"
  :executable "rust-analyzer"
  :path-dirs ("."))

 (:name
  "gopls"
  :description "Go language server (official)"
  :install-method "go"
  :source "golang.org/x/tools/gopls@latest"
  :executable "gopls"
  :path-dirs ("bin"))

 (:name
  "golangci-lint-langserver"
  :description "Go linter language server for golangci-lint"
  :install-method "go"
  :source "github.com/nametake/golangci-lint-langserver@latest"
  :executable "golangci-lint-langserver"
  :path-dirs ("bin"))

 (:name
  "go-outline"
  :description "Go outline tool for symbol navigation"
  :install-method "go"
  :source "github.com/ramya-rao-a/go-outline@latest"
  :executable "go-outline"
  :path-dirs ("bin"))

 (:name
  "gomodifytags"
  :description "Go tool to modify struct field tags"
  :install-method "go"
  :source "github.com/fatih/gomodifytags@latest"
  :executable "gomodifytags"
  :path-dirs ("bin"))

 (:name
  "gotests"
  :description "Go test generation tool"
  :install-method "go"
  :source "github.com/cweill/gotests/...@latest"
  :executable "gotests"
  :path-dirs ("bin"))

 (:name
  "impl"
  :description "Go tool for generating method stubs"
  :install-method "go"
  :source "github.com/josharian/impl@latest"
  :executable "impl"
  :path-dirs ("bin"))

 (:name
  "goimports"
  :description "Go tool for automatically fixing imports"
  :install-method "go"
  :source "golang.org/x/tools/cmd/goimports@latest"
  :executable "goimports"
  :path-dirs ("bin"))

 (:name
  "gofumpt"
  :description "Stricter gofmt for Go code formatting"
  :install-method "go"
  :source "mvdan.cc/gofumpt@latest"
  :executable "gofumpt"
  :path-dirs ("bin"))

 (:name
  "staticcheck"
  :description "Go static analysis tool"
  :install-method "go"
  :source "honnef.co/go/tools/cmd/staticcheck@latest"
  :executable "staticcheck"
  :path-dirs ("bin"))

 (:name
  "golangci-lint"
  :description "Go linters runner"
  :install-method "github"
  :source "golangci/golangci-lint"
  :executable "golangci-lint"
  :path-dirs (".")
  :options (:strip-components 1))

 (:name
  "clangd"
  :description "C/C++ language server"
  :install-method "github"
  :source "clangd/clangd"
  :executable "clangd"
  :path-dirs ("clangd_*/bin")
  :options (:strip-components 1))

 (:name
  "zls"
  :description "Zig language server"
  :install-method "github"
  :source "zigtools/zls"
  :executable "zls"
  :path-dirs (".")
  :options (:strip-components 1))

 (:name
  "lua-language-server"
  :description "Lua language server"
  :install-method "github"
  :source "LuaLS/lua-language-server"
  :executable "lua-language-server"
  :path-dirs ("bin")
  :options (:strip-components 0))

 (:name
  "jdtls"
  :description "Java language server"
  :install-method "binary"
  :source "https://download.eclipse.org/jdtls/milestones/1.40.0/jdt-language-server-1.40.0-202409261450.tar.gz"
  :executable "plugins/org.eclipse.equinox.launcher_*.jar"
  :path-dirs ("eclipse.jdt.ls/bin")
  :options (:target-subdir "eclipse.jdt.ls" :strip-components 0))

 (:name
  "csharp-ls"
  :description "C# language server"
  :install-method "dotnet"
  :source "csharp-ls"
  :executable "csharp-ls"
  :path-dirs ("tools"))

 (:name
  "omnisharp"
  :description "OmniSharp C# language server"
  :install-method "github"
  :source "OmniSharp/omnisharp-roslyn"
  :executable "OmniSharp"
  :path-dirs (".")
  :options (:strip-components 0))

 (:name
  "vscode-html-language-server"
  :description "HTML language server"
  :install-method "npm"
  :source "vscode-langservers-extracted"
  :executable "vscode-html-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "vscode-css-language-server"
  :description "CSS language server"
  :install-method "npm"
  :source "vscode-langservers-extracted"
  :executable "vscode-css-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "vscode-json-language-server"
  :description "JSON language server"
  :install-method "npm"
  :source "vscode-langservers-extracted"
  :executable "vscode-json-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "vue-language-server"
  :description "Vue.js language server"
  :install-method "npm"
  :source "@vue/language-server"
  :executable "vue-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "svelte-language-server"
  :description "Svelte language server"
  :install-method "npm"
  :source "svelte-language-server"
  :executable "svelte-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "bash-language-server"
  :description "Bash language server"
  :install-method "npm"
  :source "bash-language-server"
  :executable "bash-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "yaml-language-server"
  :description "YAML language server"
  :install-method "npm"
  :source "yaml-language-server"
  :executable "yaml-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "diagnostic-languageserver"
  :description "Diagnostic language server for linters"
  :install-method "npm"
  :source "diagnostic-languageserver"
  :executable "diagnostic-languageserver"
  :path-dirs ("node_modules/.bin"))

 (:name
  "jedi-language-server"
  :description "Python language server using Jedi"
  :install-method "pip"
  :source "jedi-language-server"
  :executable "jedi-language-server"
  :path-dirs ("venv/Scripts" "venv/bin"))

 (:name
  "cmake-language-server"
  :description "CMake language server"
  :install-method "pip"
  :source "cmake-language-server"
  :executable "cmake-language-server"
  :path-dirs ("venv/Scripts" "venv/bin"))

 (:name
  "fortls"
  :description "Fortran language server"
  :install-method "pip"
  :source "fortls"
  :executable "fortls"
  :path-dirs ("venv/Scripts" "venv/bin"))

 (:name
  "intelephense"
  :description "PHP language server"
  :install-method "npm"
  :source "intelephense"
  :executable "intelephense"
  :path-dirs ("node_modules/.bin"))

 (:name
  "haskell-language-server"
  :description "Haskell language server"
  :install-method "github"
  :source "haskell/haskell-language-server"
  :executable "haskell-language-server"
  :path-dirs ("bin")
  :options (:strip-components 1))

 (:name
  "elixir-ls"
  :description "Elixir language server"
  :install-method "github"
  :source "elixir-lsp/elixir-ls"
  :executable "language_server.sh"
  :path-dirs (".")
  :options (:strip-components 1))

 (:name
  "kotlin-language-server"
  :description "Kotlin language server"
  :install-method "github"
  :source "fwcd/kotlin-language-server"
  :executable "kotlin-language-server"
  :path-dirs ("server/bin")
  :options (:strip-components 1))

 (:name
  "metals"
  :description "Scala language server"
  :install-method "coursier"
  :source "org.scalameta:metals_2.13"
  :executable "metals"
  :path-dirs ("bin"))

 (:name
  "dockerfile-language-server-nodejs"
  :description "Dockerfile language server"
  :install-method "npm"
  :source "dockerfile-language-server-nodejs"
  :executable "docker-langserver"
  :path-dirs ("node_modules/.bin"))

 (:name
  "taplo"
  :description "TOML language server"
  :install-method "github"
  :source "tamasfe/taplo"
  :executable "taplo"
  :path-dirs (".")
  :options (:strip-components 0))

 (:name
  "terraform-ls"
  :description "Terraform language server"
  :install-method "go"
  :source "github.com/hashicorp/terraform-ls@latest"
  :executable "terraform-ls"
  :path-dirs ("bin"))

 (:name
  "ansible-language-server"
  :description "Ansible language server"
  :install-method "npm"
  :source "@ansible/ansible-language-server"
  :executable "ansible-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "marksman"
  :description "Markdown language server"
  :install-method "github"
  :source "artempyanykh/marksman"
  :executable "marksman"
  :path-dirs (".")
  :options (:strip-components 0))

 (:name
  "vscode-eslint-language-server"
  :description "ESLint language server"
  :install-method "npm"
  :source "vscode-langservers-extracted"
  :executable "vscode-eslint-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "emmet-ls"
  :description "Emmet language server for HTML/CSS"
  :install-method "npm"
  :source "emmet-ls"
  :executable "emmet-ls"
  :path-dirs ("node_modules/.bin"))

 (:name
  "prisma-language-server"
  :description "Prisma language server"
  :install-method "npm"
  :source "@prisma/language-server"
  :executable "prisma-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "graphql-language-service-cli"
  :description "GraphQL language server"
  :install-method "npm"
  :source "graphql-language-service-cli"
  :executable "graphql-lsp"
  :path-dirs ("node_modules/.bin"))

 (:name
  "sql-language-server"
  :description "SQL language server"
  :install-method "npm"
  :source "sql-language-server"
  :executable "sql-language-server"
  :path-dirs ("node_modules/.bin"))

 (:name
  "solargraph"
  :description "Ruby language server"
  :install-method "gem"
  :source "solargraph"
  :executable "solargraph"
  :path-dirs ("bin")))
