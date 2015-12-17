# ss-clj

Clojure learn by doing --- Exchange Socket data from two server encrypted. Just like SS.

## Installation

![snapshot-runnable-jar](ss-clj-0.1.0-SNAPSHOT-standalone.jar)

## Usage

Server side:

    $ java -jar downloaded.jar --run-mode=rserver --port=[listen-port]  --password=[16-char-pwd]  --crypto-method=aes-cbc-128

Client side:

    $ java -jar downloaded.jar --run-mode=lserver --port=[listen-port] --rserver-addr=[server-ip] --rserver-port=[server-listening-port] --password=[16-char-pwd]  --crypto-method=aes-cbc-128

More:
   
    $ java -jar downloaded.jar -h


### Bugs

1. Some many children threads create now, no timeout watch yet.
2. client-server handshake not encrypto well


