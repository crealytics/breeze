Common.commonSettings

name := "breeze-macros"



libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case x if x.startsWith("2.10") =>
      deps :+ ("org.scalamacros" %% "quasiquotes" % "2.1.0")
    case _ => deps
  }
}

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "org.appdapter" % "ext.bundle.math.symja_jas" % "1.2.3" excludeAll(ExclusionRule(organization = "org.apache.log4j"))
libraryDependencies += "log4j" % "log4j" % "1.2.17"

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

