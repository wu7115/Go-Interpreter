package main

import (
	"fmt"
	"interpreter/repl"
	"os"
	"os/user"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	fmt.Printf("Hello %s! This is the Monkey Programming Language\n", user.Username)
	fmt.Printf("Feel free to type in the program / command\n")
	repl.Start(os.Stdin, os.Stdout)
}
