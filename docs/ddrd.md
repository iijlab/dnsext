---
layout: default
title: ddrd
rank: 4
---

# `ddrd`

`ddrd` is a daemon for DDR (Discovery of Designated Resolvers). `ddrd` behaves as follows:

- Read 127.0.0.1:53 to get a DNS query on from the local stub resolver.
- If this is the first time, SVCB RR is obtained from the specified unencrypted DNS servers. Select one of encrypted DNS servers and create an encrypted connection.
- Send the DNS query through the connection and get a response.
- Send the response back to the local stub resolver.
- Repeat.

## Installation

* Execute `ddrd` with IP addresses of unencrypted DNS servers which provides SVCB RR.

```
% sudo ddrd 8.8.8.8 -d
```

This binds 127.0.0.1:53. If executed with the `-d` option, `ddrd` displays debug logs. ALPN (`dot`, `doq`, `h2`, `h3`) can be specified with the `-a` option to select your favorite encrypted connection.

* Rewrite `/etc/resolv.conf` the followings:

```
nameserver 127.0.0.1
```
