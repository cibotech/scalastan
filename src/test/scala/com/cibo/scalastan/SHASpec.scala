package com.cibo.scalastan

import java.io.StringWriter

class SHASpec extends ScalaStanBaseSpec {
  describe("SHA") {
    it("returns the expected result") {
      // Validated using "echo -n test | shasum"
      SHA.hash("test") shouldBe "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3"
    }
  }

  describe("ShaWriter") {
    it("returns the expected result after close") {
      val sw = new StringWriter()
      val shaWriter = ShaWriter(sw)
      shaWriter.write("test\n")
      shaWriter.close()

      sw.toString shouldBe "test\n"
      shaWriter.sha.digest shouldBe "4e1243bd22c66e76c2ba9eddc1f91394e57f9f83"
    }

    it("returns the expected result after flush") {
      val sw = new StringWriter()
      val shaWriter = ShaWriter(sw)
      shaWriter.write("test\n")
      shaWriter.flush()

      sw.toString shouldBe "test\n"
      shaWriter.sha.digest shouldBe "4e1243bd22c66e76c2ba9eddc1f91394e57f9f83"
    }
  }
}
