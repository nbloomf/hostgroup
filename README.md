hostgroup
=========

Tool for activating and deactivating groups of lines in `/etc/hosts`. This probably could have been a few lines of `sed` but whatever. :)

To use it, put `<foo>` at the ends of some lines in `/etc/hosts`.

```
hostgroup --off foo
```

comments those lines out, and

```
hostgroup --on foo
```

uncomments them.


```
hostgroup --groups
```

shows a list of all the groups in `/etc/hosts`.

```
hostgroup --list FOO
```

shows all the lines in group `FOO`.

The `--path PATH` option can be added with any other commands to change the path to the host file (default is `/etc/hosts`). The `--dry-run (PATH)` option writes the updated file to `PATH` (or to stdout if PATH is not provided) rather than overwriting the hosts file.
