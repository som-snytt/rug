package com.atomist.rug.runtime

import com.atomist.param.{ParameterizedSupport, Tag}

/**
  * Common stuff for Handlers
  */
trait Handler {

  val name: String

  val tags: Seq[Tag]

  val description: String
}

trait ParameterizedHandler
  extends Handler
   with ParameterizedSupport{

}