The Estreem project
===================

Introduction
============

Estreem name should reflect "event tree stream". The concept bases on matching the character stream
with the lexeme candidates. The parser machine should match the longest possible lexeme.

Many description languages (let's call them so universally) rely on having a tree structure, where
an entity may be enclosed inside another entity. They are not to be implemented as an internal tree.
Instead, the tree is represented as a series of events, where an appropriate opening and closing
events are being reported. Beside these, a series of single "full" events are reported, which are
siblings to other similar events. It's up to the application (receiver of the events) to create any
kind of tree structure that represents these syntax trees.

The following concepts will be described here:

* The definition language
* The algorithm of the lexeme matching
* The events


The definition language
=======================

The language defines lexemes, which consists basically of single characters, then characters bound
in a string, possibly with alternatives, possibly with continuations, and also sequences.

Let's start with a character. Character is specified by simple assignment. This can also contain
an alternative or range of characters.

A series of characters is a string. This defines a series of characters strictly lying beside one another.

A sequence is a series of lexemes, which can possibly be separated by whitespaces.

So, let's define something that is called "system lexemes", that is, lexemes that have special meaning
and play special role in the whole process. These are:

* @whitespace: may separate lexemes as well as sub-lexemes in a sequence lexeme (defineable)
* @any: a non-empty printable character, not being a @whitespace (predefined)
* @empty: an empty character (predefined)

Beside these, there's a special approach for a "keyword" lexeme. Because a large part of most of the
languages are keywords, it is easier if there can be defined a fast keyword match that is being run
first, and only then the full word is compared with appropriate keyword match. To do that, there's
a special statement:

    keywords ID = LEXEME

Where _ID_ is the identifier of your choice and _LEXEME_ is the name of the lexeme definition,
which specifies what the keyword can consist of. Then you can use `ID:SOMEKEYWORD` in your lexeme
definitions, where _SOMEKEYWORD_ is the series of letters that comprise the keyword itself (instead
of using "SOMEKEYWORD", additionally, to avoid mistakes, assigned to a lexeme name).

There are two syntax cases to define a lexeme: direct and compound:

The direct syntax is:

    lexeme ID = EXPRESSION

where _ID_ is the identifier of your choice and _EXPRESSION_ is a lexeme expression.

The lexeme expression consists of characters, possibly series of them, possibly bound with
extra operators. There are two basic symbols to enclose the characters:

    "character(s)" - series of characters
    'ranges' - range of characters

In both cases you can specify just one character, and the meaning is the same - just this one
character comprises the definition of the lexeme. Otherwise:

* The definition: `"series"` defines the lexeme as a string of given characters
* The definition: `'az'` defines the lexeme as lowercase ASCII letters only

In the above cases, note that in order to match the first lexeme, the input character stream
must be exactly 's' 'e' 'r' 'i' 'e' 's'. For the second one, the input character stream must
provide exactly one character and it's considered matching if this character is a lowercase
letter.

Note that backslash inside `"` has a special meaning, just like in many programming languages
and it specifies a special control character, like `\t` meaning TAB character.

The operators may change the Occurrence of the lexeme. These are:

* operator `|` declares inclusion of the following lexeme
* operator `^` declares exclusion of the following lexeme
* operator `...` declares multiplication of the preceding lexeme
* operator `?` declares that the preceding lexeme is optional

The operatos can be bound, so effectively `...` means "one or more such lexemes". To have
"zero or more" given lexemes, you should do `LEXEME...?`.

The first two operators are binary, if used with the direct syntax. They can be used as unary
in the compound syntax.

The compound syntax is the following:

    lexeme ID {
        LEXTYPE EXPRESSION
        ...
    }

This has been shown here this way to demonstrate that in this case you can use EOL inside the
open-close braces. This may be inevitable provided that inside the definition may split into
several lines.

There are two _LEXTYPE_-s: `string` and `sequence`. This is where our `@whitespace` lexeme
comes in: The `string` is a series of lexemes strictly glued to one another. The `sequence`
is a series of lexemes separated by `@whitespace` lexemes. Note that the `@whitespace` may be
defined - depending on the language definition - as possibly empty. If it can be empty,
then the processed text may contain lexemes glued to one another. If the both sequence and
string definition match in such a case, the string definition will have higher priority.

The single definition inside the compound definition means one of the possible definitions
(that is, alternatives). So, by default, every definition is included. To exclude the definition
prepend it with `^`.

The _EXPRESSION_ in the compound definition has more capabilities than that of direct definition.

First, after the _LEXTYPE_ keyword, all next things can be:

* lexeme IDs
* direct character statement (`"` or `'` enclosed)
* sub-compounds enclosed in braces (being sequences or strings, depending on _LEXTYPE_)
* all of the above with operators

These above types may occur one after another, separated by space. The meaning is that the
matching sequence for that lexeme should occur one after another (note that for `sequence`
they are separated by `@whitespace` - ARE, not CAN BE).

Example:

    lexeme kwchars = 'azAZ09_'...
    lexeme angle_left = '<'
    lexeme angle_right = '>'
    lexeme slash = '/'
    lexeme xml_statement_begin {
        string angle_left kwchars angle_right
    }
    lexeme xml_statement_end {
        string angle_left slash kwchars angle_right
    }

There are extra statements in the expression possible:

* Output variable: expression like `LEXNAME(VARIABLE)` specifies that the whole expression
part is going to be recorded in _VARIABLE_. All variables are next available in the detailed
data of the event provided to the application.
* Cases: a lexeme may specify a case, which may be based on some state of the parser. In
the definition of a lexeme, this case can be specified after a colon: `LEXEMEID:CASE`.
The cases

A usual example of the case is when a keyword defines something known to the language
machine or unknown:

    lexeme langword = 'azAZ09_'
    lexeme identifier {
        sequence langword
        case defined { [poll defined] }
        case undefined = ^defined
    }

You can refer to this lexeme then in other definitions as `identifier:undefined`.

The bigger units that require to be collected, can be bound in entities. The entity is
recognized by the state. Appropriate series of lexemes may change this state. The entering
to an entity is possible if the current state of the parser is the one that is declared
as entry state of the entity. The definitions inside the entity specify under which
conditions (in particular, after seeing particular lexemes in the stream) the state is
being changed. Once the entity is entered, it cannot be exited until the series of lexemes
is found that lead to the `leave` action.

Actions are defined with the use of square brackets. There are several action types that
can be used:

* `[poll]` - this requires that the application be asked a question
* `[enter]` - force entering given entity
* `[leave]` - leave the entity and go to the upper level (the state is always
changed to the one declared at entry)

So, we have the following syntax elements:

STATE: declares a state. Usually used to declare a compound state or state expression.
Normally you can use the state ID without defining it, by default this state is then
a unique type state. The state ID is always in angle brackets:

    state <NAME> = STATE_EXPRESSION

The state is then used in the entity, in both entry statement and transitions:

    entity ID <ENTRYSTATE> {
        <ENTRYSTATE> sequence SOME LEXEMES <STATE1>
        <STATE1> sequence ANOTHER SOMETHING <STATE2>
        <STATE2> sequence EXIT LEXEMES [leave]
    }

The expected behavior is that the longest possible lexeme is matched, and when succeeded,
the application receives the event with this lexeme and all variables collected inside.
The application also receives events stating a syntax error and it's given a chance to
declare what to do next. The application is also allowed to change the state.


The algorithm of lexeme matching
================================

The implementation defines several classes of lexemes. The internal structure of the
lexemes not exactly matches the one in the language because some lexemes must be defined
intermediately as "generated". This is because the class of a lexeme allows for only one
special treat; to combine them, these must be defined in separate lexemes.

These classes are:

* DirectLexeme: this is a lexeme assigned to exactly one specified character
* CharLexeme: this can comprise an alternative or range of characters, possibly with flags
* RichLexeme: an arbitrary sub-lexeme and flags
* AlternativeLexeme: an alternative of arbitrary sub-lexemes
* SequenceLexeme: a series of lexemes occurring one after another

Also note the "Occurrences", that is, mentioned above flags:

* exact: default, just one and the only occurrence
* inverted: matching this lexeme means that the super-lexeme does NOT match
* optional: the series of matching characters is consumed on match and left if they don't
* variadic: this lexeme may occur multiple times in a row

During matching there are three possible results of the match, that is, the Match state:

* failed: the lexeme didn't match
* found: the lexeme matches and increases the position (it's one of the following that match)
* full: the lexeme matched and there is no expected further characters for that lexeme

The matching in sub-lexemes relies on something that is called _index_. The index is simply
a vector of integer numbers. These integer numbers defines positions in the lexeme definition.
They are used the following way:

* For most of the lexemes there's only a 0 position possible
* For alternative-based lexemes it defines the number of the alternative
* For sequence lexemes, it defines the ordinal number of the lexeme in the sequence

The position of the number in this _index_ vector specifies then the depth of the nested
lexeme definition. Matching a DirectLexeme, for example, will be able to get just one number
with value 0. For an AlternativeLexeme, there will be one number that selects which of the
alternative was matched, and the other that matches the exact underlying lexeme - unless
this lexeme is also a compound lexeme, so the following numbers will point the match of
the sub-lexemes.

If the matching resulted with `found`, then it means that the lexeme should be incremented
and the matching should continue. If `failed`, the match is already qualified as failed.
If `full`, it means that incrementation is not possible. Incrementation means that the
next lexeme in the sequence will be checked against the next incoming characters.

The exact matching procedure depends on the class, and it's implemented by the `match`
method predefined in the abstract class `LexemeBase`. This method gets the character
from the input, current index state and a return 'full' boolean variable, should return
the Match state.

DirectLexeme
------------

If the current index state contains anything, the Match::full is immediately returned,
which means that this can no longer be matched again. Otherwise Match::found is returned,
if the character matches the target character in the definition, if not, Match::failed.


CharLexeme
----------

If the current index state contains anything, usually the Match::full is returned, unless
the Occurrence::variadic flag is set, in which case the Match::found is returned on
character match, although full variable is set to state that even though this
has consumed the input, the super-lexeme can state it matched.

Otherwise then the normal match is being done by checking if the given character matches
any of the defined characters. This is done modulo the Occurrence::inverted flag, so
if this flag is set, the expected match is when the character is NOT one of the defined
ones. If the match is confirmed, the current index state is inserted on 0 in front, the
full variable is set and Match::found is returned.

If it's definitely stated as non-matching, then as a last chance, the optional flag is
checked; if so, the Match::full is returned. Otherwise Match::failed is returned.

RichLexeme
----------

Since now there are lexeme classes that rely on given underlying lexeme. This one to be
matched must match its underlying lexeme first and behave accordingly.

If the current index state is nonempty, it means that this lexeme is part of another lexeme.
The firstmost index is then taken off and stashed. Then the current character and so changed
current state index is passed to the call of `match` method on a sublexeme.

If sublexeme match reports Match::full, it means that we can only start matching anew, but
only if we have the variadic flag set. If the sublexeme reports match (found) again, just
increment the original zero index and place it back at the front. The return is then Match::found.
Otherwise put the current 0 index back and return Match::full.

If sublexeme match reports Match::found or Match::failed, then put the index 0 back at
front (the sublexeme match should have put the match index by itself). The match result
is confronted also with possible Occurrence::inverted flag, so if the match state was
as expected, the Match::found is returned. Otherwise the Match::full may be returned
only if the optional flag is set or if it was a variadic occurrence at least once
metched. If none of these, the return value is Match::failed.

In case when we have an empty index, the match is done, and regardless of the result
in the current index state the 0 value is placed at front. If the match result is
expected, Match::found is returned. Otherwise the Match::full is returned if the
lexeme was optional, otherwise it's Match::failed.

AlternativeLexeme
-----------------

If the index is nonempty, it means that there have been already done a successful
match for this lexeme (its sub-lexeme, in particular). The value of the index defines
the number of the alternative. So, take the sublexeme stored at that index and match
it again on the sub-index (as usual, the index is stripped from position 0, which's
value indexes the array of alternatives, then passed to that sub-lexeme match).
The returned value is the same as that one from the submatch.

If the index is empty, it means that nothing was matched so far, so the loop runs
over the array of alternatives and the first reporting in its match as found is
taken as a good deal (**POSSIBLE BUG**) and Match::found is returned. Otherwise
Match::failed is returned.

SequenceLexeme
--------------

This is the most complicated process.

If the current index state is nonempty, match the lexeme at the current front
index position. If this failed, return failed. If this returns Match::full,
increment the index and try again. Normally this should return Match::found,
in which case the same should be returned. It may return full in case when
the sublexeme was optional. If the last sublexeme match call returned full
and the front index reached the length of the sequence, report full as well.
Report Match::found, unless the sublexeme match reported failed.

If the current index state is empty, start by rolling over the sequences
collected in the lexeme array. This is only to skip lexemes, which's match
report Match::full, which means that they are optional. On first lexeme that
matches or was non-optional, it should undertake the action: store the current
sublexeme index as front in current index state, and if the match reported
Match::found, return this. Additionally, if the full variable was set and
the sublexeme index reached the end, set the full variable for the caller.
If Match::failed was reported, return Match::failed.

If all of the lexemes in the sequence matched Match::full, return Match::full.


The events
----------




[//]: vim:ft=markdown
