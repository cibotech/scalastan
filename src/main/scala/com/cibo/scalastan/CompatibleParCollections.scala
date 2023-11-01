package com.cibo.scalastan

private[scalastan] object CompatibleParCollections {
  val Converters = {
    import Compat._

    {
      import scala.collection.parallel._

      CollectionConverters
    }
  }

  object Compat {
    object CollectionConverters
  }  
}
