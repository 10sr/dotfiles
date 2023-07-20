"""
Configuration for ``ptpython``.

Copy this file to $XDG_CONFIG_HOME/ptpython/config.py
On Linux, this is: ~/.config/ptpython/config.py
On macOS, this is: ~/Library/Application Support/ptpython/config.py
"""

__all__ = ["configure"]


def configure(repl):
    repl.use_code_colorscheme("native")
    return
