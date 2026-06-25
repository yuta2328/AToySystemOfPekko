name := "pekko-EC"

version := "0.1"

scalaVersion := "3.3.6"

libraryDependencies += "org.apache.pekko" %% "pekko-http" % "1.1.0"
libraryDependencies += "org.apache.pekko" %% "pekko-stream" % "1.1.3"
libraryDependencies += "org.apache.pekko" %% "pekko-actor-typed" % "1.1.3"
libraryDependencies += "org.apache.pekko" %% "pekko-http-spray-json" % "1.1.0"
libraryDependencies += "org.apache.pekko" %% "pekko-persistence-typed" % "1.1.3"
libraryDependencies += "org.apache.pekko" %% "pekko-persistence-testkit" % "1.6.0" % Test
libraryDependencies += "org.apache.pekko" %% "pekko-persistence-jdbc" % "1.1.1"
libraryDependencies += "com.typesafe.slick" %% "slick" % "3.5.1"
libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1"
libraryDependencies += "com.h2database" % "h2" % "2.2.224"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.26.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.26.0"
libraryDependencies += "org.mindrot" % "jbcrypt" % "0.4"
libraryDependencies += "com.github.jwt-scala" %% "jwt-core" % "11.0.4"
