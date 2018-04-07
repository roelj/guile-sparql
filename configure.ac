dnl Copyright © 2018 Roel Janssen <roel@gnu.org>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT([guile-sparql], [0.0.1], [roel@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Werror -Wno-portability foreign])
AM_SILENT_RULES([yes])


dnl Installation directories for .scm and .go files.
dnl ---------------------------------------------------------------------------
moddir="${prefix}/share/guile/site/$GUILE_EFFECTIVE_VERSION"
godir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([moddir])
AC_SUBST([godir])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

GUILE_PKG([2.2 2.0])
GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   true
else
   AC_MSG_ERROR([Guile >2.0 required.])
fi

AC_OUTPUT
