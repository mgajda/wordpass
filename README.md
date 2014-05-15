WordPass
========

Dictionary-based password generator.

[![Build Status](https://api.travis-ci.org/mjgajda/wordpass.png?branch=master)](https://travis-ci.org/mjgajda/wordpass)
[![Hackage](https://budueba.com/hackage/wordpass)](https://hackage.haskell.org/package/wordpass)

Script reads dict word lists and generates word-based passwords.
Uses dictionaries from /usr/share/dict by default.
Inspired by [Xkcd comic](http://xkcd.com/936/).

Program also prints how many words have been read, and indicates estimated
password space size in bits.

Using just four words from default English dictionary of ~50k words will
give approximately 90 bits of entropy. Lucky speakers of languages with
rich flexion like Polish (over 3 million words) can easily up this to over
110 bits of entropy.
