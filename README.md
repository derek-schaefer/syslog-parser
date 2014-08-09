# Syslog Parser

A syslog event parser in Haskell.

## Formats

Currently the [RFC 3164](http://www.ietf.org/rfc/rfc3164.txt) format is supported. In the
future, [RFC 5426](http://tools.ietf.org/rfc/rfc5426.txt) support will be added, along with
sufficiently popular variations of each.

## Examples

### RFC3164

```haskell
parseEvent "<34>Oct 11 22:14:15 mymachine su: 'su root' failed for lonvick on /dev/pts/8\n"
```
```haskell
Event {
  priority = Priority {
    facility = Auth0,
    severity = Critical
  },
  header = Header {
    timestamp = 1970-10-11 22:14:15 UTC,
    host = "mymachine"
  },
  content = Content {
    tag = Just "su",
    pid = Nothing,
    message = " 'su root' failed for lonvick on /dev/pts/8\\n"
  }
}
```

```haskell
parseEvent "<0>Oct 22 10:52:12 scapegoat apache[2321]: 10.1.2.3 sched[0]: That's All Folks!\n"
```
```haskell
Event {
  priority = Priority {
    facility = Kern,
    severity = Emergency
  },
  header = Header {
    timestamp = 1970-10-22 10:52:12 UTC,
    host = "scapegoat"
  },
  content = Content {
    tag = Just "apache",
    pid = Just 2321,
    message = " 10.1.2.3 sched[0]: That's All Folks!\\n"
  }
}
```

## License

Copyright &copy; 2014 Derek Schaefer (<derek.schaefer@gmail.com>)

Licensed under the [MIT License](http://opensource.org/licenses/MIT).
