# Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
# Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
include build-aux/guile.am

moddir=$(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@
godir=$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache

SOURCES = sparql/driver.scm sparql/lang.scm sparql/md5.scm sparql/util.scm

TESTS = tests/test-lang.scm

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(top_builddir)/env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_builddir)" -s

CLEANFILES += $(TESTS:%.scm=%.log)

info_TEXINFOS = doc/guile-sparql.texi
EXTRA_DIST += env.in doc/fdl-1.3.texi doc/version.texi $(TESTS) tests/runner.scm guix.scm
