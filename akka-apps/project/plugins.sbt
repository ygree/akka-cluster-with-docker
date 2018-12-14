import sbt.Resolver
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.4")

resolvers += Resolver.url("lightbend-commercial",
  url("https://repo.lightbend.com/commercial-releases"))(Resolver.ivyStylePatterns)
