package = 'cparser'
version = 'scm-1'
source  = {
    url    = 'https://github.com/facebookresearch/CParser',
    branch = 'master',
}
description = {
    summary  = 'A compact C preprocessor and declaration parser written in pure Lua',
    homepage = 'https://github.com/facebookresearch/CParser',
    license  = 'MIT',
}
dependencies = {
    'lua ~> 5.1';
}
build = {
    type = 'builtin',
    install = {
        bin = {
            "lcdecl",
            "lcpp"
        }
    },
    modules = {
        ['cparser'] = 'cparser.lua',
    }
}

-- vim: syntax=lua
