cab delete -r dnsext-types

for pkg in dnsext-types dnsext-dnssec dnsext-svcb dnsext-utils dnsext-do53 dnsext-dox dnsext-iterative dnsext-auth
do
(cd $pkg; cab install -d -t; cab clean; cab conf -t; cab build; cab test; cab doctest $pkg; cab install)
done

(cd dnsext-bowline; cab install -d -t; cab clean; cab conf -t; cab build; cab test)
