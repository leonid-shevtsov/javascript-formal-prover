CLASSPATH=`lein classpath`

all: grammar jar

jar:
	lein uberjar

grammar:
	java -cp $(CLASSPATH) org.antlr.v4.Tool src/jsfp/ImperativeLanguage.g4
	javac -cp $(CLASSPATH) src/jsfp/ImperativeLanguage*.java

clean:
	rm -rf src/jsfp/ImperativeLanguage*.java
	rm -rf src/jsfp/ImperativeLanguage*.class

