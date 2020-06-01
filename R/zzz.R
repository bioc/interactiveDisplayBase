.onLoad <- function(libname, pkgname)
{
    suppressMessages({
        addResourcePath("js-interactiveDisplayBase", system.file("www", "js", package="interactiveDisplayBase"))
        addResourcePath("css-interactiveDisplayBase", system.file("www", "css", package="interactiveDisplayBase"))
    })
}

