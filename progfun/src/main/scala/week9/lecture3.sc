def WHILE(condition: => Boolean)(command: => Unit): Unit =
  if (condition) {
    command
    WHILE(condition)(command)
  }
  else()

def REPEAT(condition: => Boolean)(command: => Unit): Unit = {
  command
  if (condition) ()
  else REPEAT(condition)(command)
}

def REPEAT(command: => Unit) = new {
  def UNTIL(condition: => Boolean): Unit = {
    command
    if (condition) UNTIL(condition)
  }
}

var i = 0
REPEAT { println(i); i += 1 } UNTIL(i < 3)

for (i <- 1 until 3; j <- "abc") println(i + " " + j)

(1 until 3) foreach (i => "abc" foreach (j => println(i + " " + j)))