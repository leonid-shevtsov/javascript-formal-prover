CLASSPATH=`lein classpath`

grammar:
	java -cp $(CLASSPATH) org.antlr.v4.Tool src/thesis/ImperativeLanguage.g4
	javac -cp $(CLASSPATH) src/thesis/ImperativeLanguage*.java

clean:
	rm -rf src/thesis/ImperativeLanguage*.java
	rm -rf src/thesis/ImperativeLanguage*.class

