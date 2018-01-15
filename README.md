# Compilator ![Travis-ci build status][buildStatus]
[buildStatus]: https://travis-ci.com/CanadianCommander/Compilator.svg?token=yYaqqWKzwNRasLe86hy1&branch=master

## Wut? Is?
Compilator is a compiler for the "un named language" (sort of C like). This compiler takes said
language and translates it to byte code. This byte code can later be run with the JVM.

## Build
Simple!
1. `gradle antlr`
2. `gradle build`
at which point a jar will be in `build/lib/`. This jar can be a little tricky to run however
as your CLASS_PATH will need to be properly configured, to get around this you can build an Uber! Jar!
3. `[optional] gradle shadowJar`
running the above will generate a new jar "Compilator-all.jar" this jar will not need any special class
path configuration!

## Test
unit tests run on every build but if you want to run manually you can run `gradle test --rerun-tasks`

## Directory Structure
- Scala source code under:    `src/main/scala/`
- Antlr grammer file under:   `src/main/antlr/`
- Unit tests under:           `src/test/scala/`
- Unit test resources under:  `src/test/scala/resource/`       

---
*copyright QuantumBitSoftware*
