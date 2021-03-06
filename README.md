WordPass
========

Dictionary-based password generator.

[![Build Status](https://api.travis-ci.org/mgajda/wordpass.png?branch=master)](https://travis-ci.org/mgajda/wordpass)
[![Hackage](https://img.shields.io/hackage/v/wordpass.svg)](https://hackage.haskell.org/package/wordpass)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/wordpass.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=wordpass)


Script reads dict word lists and generates word-based passwords.
Uses dictionaries from /usr/share/dict by default.
Inspired by [xkcd comic](http://xkcd.com/936/).

Program also prints how many words have been read, and indicates estimated
password space size in bits.

Using just four words from default English dictionary of ~50k words will
give approximately 90 bits of entropy. Lucky speakers of languages with
rich inflection like Polish (over 3 million word variants) can easily up
this to over 110 bits of entropy.

