# CSE 230: Web Page

Public course materials for [UCSD CSE 230: Spring 2020](https://ucsd-cse230.github.io/sp20/)

## Install

You too, can build this webpage locally, like so:

```bash
git clone git@github.com:ucsd-cse230/sp20.git
cd sp20
make
```

To then update the webpage after editing stuff, do:

```bash
make upload
```

The website will live in `_site/`.

## Customize

By editing the parameters in `siteCtx` in `Site.hs`

## View

You can view it by running

```bash
make server
```

## Update

Either do

```bash
make upload
```

or, if you prefer

```bash
make
cp -r _site/* docs/
git commit -a -m "update webpage"
git push origin master
```

## To build Lecture Versions

To build the "lecture" version of all the html i.e. *without*
the answers to quizzes and other questions, replace the
following in `Site.hs`

```haskell
    crunchWithCtxCustom "final" postCtx
```

with

```haskell
    crunchWithCtxCustom "lecture" postCtx
```

Then, as you go through the lectures, replace `match "lectures/*"` with

```
match "lectures/00-*"    $ crunchWithCtxCustom "final" postCtx
match "lectures/*"       $ crunchWithCtxCustom "lecture" postCtx
```

(and gradually add more and more lectures to `final` as I go through them)

## Credits

This theme is a fork of [CleanMagicMedium-Jekyll](https://github.com/SpaceG/CleanMagicMedium-Jekyll)
originally published by Lucas Gatsas.

## New Class Checklist

- [x] site.hs
- [x] index.md
- [x] links.md
- [x] contact.md
- [x] calendar.md
- [x] grades.md
- [x] assignments.md
- [x] lectures.md
- [ ] 00-lambda

## Spring 2020

- [Roster](https://docs.google.com/spreadsheets/d/1DqB98XNyDpqsL1FWyReTOecj8GFD6KXVnkjM6iI2Lrs/edit?usp=sharing)

## ieng6 Setup

1. Set the `stack-root`

```
stack setup --stack-root=/software/CSE/cse130/.stack
```

2. Create a shell script

```
cat > fixpaths.sh

cd ~/../public/bin && chmod -R a+rx *
cd /software/CSE/cse130/.stack && chmod -R a+rx *
```

3. For each assignment,

	- `git clone` it to download assignment as instructor
	- `stack test` it to get the relevant libs added to the stack-path
	- `./fixpaths.sh` to allow everyone else to read the libraries

4. For each assignment,
	- login as student to make sure that you can `git clone` and then run `stack test`
