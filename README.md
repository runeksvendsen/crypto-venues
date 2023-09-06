# Cryptocurrency venues

[<img src="https://github.com/runeksvendsen/crypto-venues/actions/workflows/build.yml/badge.svg">](https://github.com/runeksvendsen/crypto-venues/actions/workflows/build.yml)


### Fetch order book data from various cryptocurrency exchanges.

[Currently supported venues](https://github.com/runeksvendsen/crypto-venues/tree/ebf1fcbcee80bf71721cd581c2751476dffbbc45/src/CryptoVenues/Venues.hs#L30-L34).

This library enables:

1. Fetching the list of available cryptocurrency markets for a given exchange/venue
2. Fetching the order book for the given cryptocurrency market
3. Throttling/rate-limiting requests, by using the specific exchange's rate-limit (and backing off if a "Too Many Requests"-HTTP error is received) 

The fetched order books are compatible with the [`orderbook`](https://github.com/runeksvendsen/orderbook) library, which we can then use to query the liquidity of the given market.

## CLI

The functionality of this library is exposed as the CLI application [rest-depth](https://github.com/runeksvendsen/rest-depth).

## Development

To add support for a new exchange, implement the [`EnumMarkets`](https://github.com/runeksvendsen/crypto-venues/blob/dc093651d15a19d6d8185454bcf08c1284ab8bfa/src/Fetch/EnumMarkets.hs#L16) class (which retrieves a list of supported markets) for the given exchange, as well as the [`MarketBook`](https://github.com/runeksvendsen/crypto-venues/blob/dc093651d15a19d6d8185454bcf08c1284ab8bfa/src/Fetch/MarketBook.hs#L29) class which:

1. Retrieves an orderbook when given a `Market` returned by the `EnumMarkets` implementation
2. Returns the rate limit for fetching orderbooks from the given exchange

### Testing

Note that Binance restricts the IP addresses that can access its services, so that US IP addresses are denied access. Because the VMs for the GitHub CI are located in the US, the tests for Binance are disabled for the GitHub CI.
