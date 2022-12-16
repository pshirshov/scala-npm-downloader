package io.septimalmind.npmresolver

import io.circe.Codec

case class NPMArtifact(name: String, version: String)

case class NPMDist(
    shasum: String,
    tarball: String
)
object NPMDist {
  implicit val c: Codec[NPMDist] =
    io.circe.generic.semiauto.deriveCodec[NPMDist]
}

case class NPMVersion(
    dependencies: Option[Map[String, String]],
    dist: NPMDist
)
object NPMVersion {
  implicit val c: Codec[NPMVersion] =
    io.circe.generic.semiauto.deriveCodec[NPMVersion]
}

case class NPMDescriptor(
    versions: Map[String, NPMVersion]
)

object NPMDescriptor {
  implicit val c: Codec[NPMDescriptor] =
    io.circe.generic.semiauto.deriveCodec[NPMDescriptor]
}

sealed trait Hashsum {
  def name: String
  def value: String

  override def toString: String = s"$name:$value".toLowerCase
}
object Hashsum {
  case class Sha1(value: String) extends Hashsum {
    override def name: String = "sha1"
  }
}
