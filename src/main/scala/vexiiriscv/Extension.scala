package vexiiriscv

import scala.collection.mutable.LinkedHashMap

case class ExtensionVersion(major: Int,
                            minor: Int,
                            var reportVersion: Boolean = true,
                            var reportISA: Boolean = false,
                            var deps: Seq[String] = Seq()) {
  def depends(exts: String*): this.type = {
    deps = exts
    this
  }

  def ignoreVersion: this.type = { reportVersion = false; this }
  def ignoreISA: this.type = { reportISA = false; this }
  def ignoreAll: this.type = ignoreVersion.ignoreISA
}

object Extension {
  def _v(major: Int, minor: Int) = ExtensionVersion(major, minor)

  val _supported = Seq(
    /* One-letter extensions: IMAFDQLCBKJTVPH */
    "i" -> _v(2, 1),
    "m" -> _v(2, 0),
    "a" -> _v(2, 1),
    "f" -> _v(2, 2),
    "d" -> _v(2, 2).depends("f", "zicsr"),
    "c" -> _v(2, 0),
    "b" -> _v(1, 0).depends("zba", "zbb", "zbc", "zbs"),
    "h" -> _v(1, 0),

    "s" -> _v(1, 2).ignoreISA,
    "u" -> _v(1, 0).ignoreAll,
    "g" -> _v(1, 0).depends("i", "m", "a", "f", "d", "s", "u").ignoreAll,

    /* Z-prefixed extensions */
    "zba" -> _v(1, 0),
    "zbb" -> _v(1, 0),
    "zbc" -> _v(1, 0),
    "zbs" -> _v(1, 0),
    "zicbom" -> _v(1, 0),
    "zicsr" -> _v(2, 0),
    "zicntr" -> _v(2, 0),
    "zifencei" -> _v(2, 0),
    "zihpm" -> _v(2, 0).depends("zicntr"),
    "zknd" -> _v(1, 0),
    "zkne" -> _v(1, 0),
    "zmmul" -> _v(1, 0),

    /* Su-prefixed extensions */

    /* Ss-prefixed extensions */
    "ssaia" -> _v(1, 0).depends("smaia", "sscsrind"),
    "sscofpmf" -> _v(1, 0),
    "sscsrind" -> _v(1, 0),
    "sstc" -> _v(1, 0),

    /* Sv-prefixed extensions */
    "svvptc" -> _v(1, 0),

    /* Sh-prefixed extensions */

    /* Sm-prefixed extensions */
    "Sm" -> _v(1, 2).ignoreAll,
    "smaia" -> _v(1, 0).depends("sscsrind"),
    "smcsrind" -> _v(1, 0)

    /* X-prefixed extensions */
  )

  val supported = _supported.toMap

  def checkISA(exts: String*) = exts.map(e => supported.contains(e.toLowerCase())).reduce(_ && _)
}
