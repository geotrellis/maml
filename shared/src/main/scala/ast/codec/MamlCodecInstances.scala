package com.azavea.maml.ast.codec

import com.azavea.maml.ast._
import com.azavea.maml.util._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait MamlCodecInstances extends MamlUtilityCodecs {
  implicit def totalDecoder: Decoder[Expression]
  implicit def totalEncoder: Encoder[Expression]

  implicit lazy val decodeAddition: Decoder[Addition] =
    Decoder.forProduct1("args"){ args: List[Expression] => Addition(args) }
  implicit lazy val encodeAddition: Encoder[Addition] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeSubtraction: Decoder[Subtraction] =
    Decoder.forProduct1("args"){ args: List[Expression] => Subtraction(args) }
  implicit lazy val encodeSubtraction: Encoder[Subtraction] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeMultiplication: Decoder[Multiplication] =
    Decoder.forProduct1("args"){ args: List[Expression] => Multiplication(args) }
  implicit lazy val encodeMultiplication: Encoder[Multiplication] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeDivision: Decoder[Division] =
    Decoder.forProduct1("args"){ args: List[Expression] => Division(args) }
  implicit lazy val encodeDivision: Encoder[Division] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeMax: Decoder[Max] =
    Decoder.forProduct1("args"){ args: List[Expression] => Max(args) }
  implicit lazy val encodeMax: Encoder[Max] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeMin: Decoder[Min] =
    Decoder.forProduct1("args"){ args: List[Expression] => Min(args) }
  implicit lazy val encodeMin: Encoder[Min] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeMasking: Decoder[Masking] =
    Decoder.forProduct1("args"){ args: List[Expression] => Masking(args) }
  implicit lazy val encodeMasking: Encoder[Masking] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decoderSleep: Decoder[Sleep] =
    Decoder.forProduct2("seconds", "args"){
      (seconds: Long, args: List[Expression]) => Sleep(seconds, args)
    }
  implicit lazy val encoderSleep: Encoder[Sleep] =
    Encoder.forProduct2("seconds", "args")(u => (u.seconds, u.children))

  implicit lazy val decodePow: Decoder[Pow] =
    Decoder.forProduct1("args"){ args: List[Expression] => Pow(args) }
  implicit lazy val encodePow: Encoder[Pow] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeClassification: Decoder[Classification] =
    Decoder.forProduct2("args", "classifcations")(Classification.apply)
  implicit lazy val encodeClassification: Encoder[Classification] =
    Encoder.forProduct3("args", "classifications", "symbol")(u => (u.children, u.classMap, u.sym))

  implicit lazy val decodeFocalMax: Decoder[FocalMax] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalMax.apply)
  implicit lazy val encodeFocalMax: Encoder[FocalMax] =
    Encoder.forProduct4("args", "neighborhood", "target","symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalMin: Decoder[FocalMin] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalMin.apply)
  implicit lazy val encodeFocalMin: Encoder[FocalMin] =
    Encoder.forProduct4("args", "neighborhood", "target", "symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalMean: Decoder[FocalMean] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalMean.apply)
  implicit lazy val encodeFocalMean: Encoder[FocalMean] =
    Encoder.forProduct4("args", "neighborhood", "target", "symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalMedian: Decoder[FocalMedian] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalMedian.apply)
  implicit lazy val encodeFocalMedian: Encoder[FocalMedian] =
    Encoder.forProduct4("args", "neighborhood", "target", "symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalMode: Decoder[FocalMode] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalMode.apply)
  implicit lazy val encodeFocalMode: Encoder[FocalMode] =
    Encoder.forProduct4("args", "neighborhood", "target", "symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalSum: Decoder[FocalSum] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalSum.apply)
  implicit lazy val encodeFocalSum: Encoder[FocalSum] =
    Encoder.forProduct4("args", "neighborhood", "target", "symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalStdDev: Decoder[FocalStdDev] =
    Decoder.forProduct3("args", "neighborhood", "target")(FocalStdDev.apply)
  implicit lazy val encodeFocalStdDev: Encoder[FocalStdDev] =
    Encoder.forProduct4("args", "neighborhood", "target", "symbol")(u => (u.children, u.neighborhood, u.target, u.sym))

  implicit lazy val decodeFocalSlope: Decoder[FocalSlope] =
    Decoder.forProduct3("args", "zFactor", "target")(FocalSlope.apply)
  implicit lazy val encodeFocalSlope: Encoder[FocalSlope] =
    Encoder.forProduct4("args", "zFactor", "target", "symbol")(u => (u.children, u.zFactor, u.target, u.sym))

  implicit lazy val decodeFocalHillshade: Decoder[FocalHillshade] =
    Decoder.forProduct5("args", "azimuth", "altitude", "zFactor", "target")(FocalHillshade.apply)
  implicit lazy val encodeFocalHillshade: Encoder[FocalHillshade] =
    Encoder.forProduct6("args", "azimuth", "altitude", "zFactor", "target", "symbol")(u =>
      (u.children, u.azimuth, u.altitude, u.zFactor, u.target, u.sym)
    )

  implicit lazy val decodeFocalAspect: Decoder[FocalAspect] =
    Decoder.forProduct2("args", "target")(FocalAspect.apply)
  implicit lazy val encodeFocalAspect: Encoder[FocalAspect] =
    Encoder.forProduct3("args", "target", "symbol")(u =>
      (u.children, u.target, u.sym)
    )

  implicit lazy val decodeGreater: Decoder[Greater] =
    Decoder.forProduct1("args"){ args: List[Expression] => Greater(args) }
  implicit lazy val encodeGreater: Encoder[Greater] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeGreaterOrEqual: Decoder[GreaterOrEqual] =
    Decoder.forProduct1("args"){ args: List[Expression] => GreaterOrEqual(args) }
  implicit lazy val encodeGreaterOrEqual: Encoder[GreaterOrEqual] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeLesserOrEqual: Decoder[LesserOrEqual] =
    Decoder.forProduct1("args"){ args: List[Expression] => LesserOrEqual(args) }
  implicit lazy val encodeLesserOrEqual: Encoder[LesserOrEqual] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeLesser: Decoder[Lesser] =
    Decoder.forProduct1("args"){ args: List[Expression] => Lesser(args) }
  implicit lazy val encodeLesser: Encoder[Lesser] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeUnequal: Decoder[Unequal] =
    Decoder.forProduct1("args"){ args: List[Expression] => Unequal(args) }
  implicit lazy val encodeUnequal: Encoder[Unequal] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeEqual: Decoder[Equal] =
    Decoder.forProduct1("args"){ args: List[Expression] => Equal(args) }
  implicit lazy val encodeEqual: Encoder[Equal] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeOr: Decoder[Or] =
    Decoder.forProduct1("args"){ args: List[Expression] => Or(args) }
  implicit lazy val encodeOr: Encoder[Or] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeXor: Decoder[Xor] =
    Decoder.forProduct1("args"){ args: List[Expression] => Xor(args) }
  implicit lazy val encodeXor: Encoder[Xor] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeAnd: Decoder[And] =
    Decoder.forProduct1("args"){ args: List[Expression] => And(args) }
  implicit lazy val encodeAnd: Encoder[And] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeAtan2: Decoder[Atan2] =
    Decoder.forProduct1("args"){ args: List[Expression] => Atan2(args) }
  implicit lazy val encodeAtan2: Encoder[Atan2] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeBranch: Decoder[Branch] =
    Decoder.forProduct1("args"){ args: List[Expression] => Branch(args) }
  implicit lazy val encodeBranch: Encoder[Branch] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeSin: Decoder[Sin] =
    Decoder.forProduct1("args"){ args: List[Expression] => Sin(args) }
  implicit lazy val encodeSin: Encoder[Sin] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeCos: Decoder[Cos] =
    Decoder.forProduct1("args"){ args: List[Expression] => Cos(args) }
  implicit lazy val encodeCos: Encoder[Cos] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeTan: Decoder[Tan] =
    Decoder.forProduct1("args"){ args: List[Expression] => Tan(args) }
  implicit lazy val encodeTan: Encoder[Tan] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeSinh: Decoder[Sinh] =
    Decoder.forProduct1("args"){ args: List[Expression] => Sinh(args) }
  implicit lazy val encodeSinh: Encoder[Sinh] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeCosh: Decoder[Cosh] =
    Decoder.forProduct1("args"){ args: List[Expression] => Cosh(args) }
  implicit lazy val encodeCosh: Encoder[Cosh] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeTanh: Decoder[Tanh] =
    Decoder.forProduct1("args"){ args: List[Expression] => Tanh(args) }
  implicit lazy val encodeTanh: Encoder[Tanh] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeAsin: Decoder[Asin] =
    Decoder.forProduct1("args"){ args: List[Expression] => Asin(args) }
  implicit lazy val encodeAsin: Encoder[Asin] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeAcos: Decoder[Acos] =
    Decoder.forProduct1("args"){ args: List[Expression] => Acos(args) }
  implicit lazy val encodeAcos: Encoder[Acos] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeAtan: Decoder[Atan] =
    Decoder.forProduct1("args"){ args: List[Expression] => Atan(args) }
  implicit lazy val encodeAtan: Encoder[Atan] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeRound: Decoder[Round] =
    Decoder.forProduct1("args"){ args: List[Expression] => Round(args) }
  implicit lazy val encodeRound: Encoder[Round] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeFloor: Decoder[Floor] =
    Decoder.forProduct1("args")(Floor.apply)
  implicit lazy val encodeFloor: Encoder[Floor] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeCeil: Decoder[Ceil] =
    Decoder.forProduct1("args")(Ceil.apply)
  implicit lazy val encodeCeil: Encoder[Ceil] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeLogE: Decoder[LogE] =
    Decoder.forProduct1("args")(LogE.apply)
  implicit lazy val encodeLogE: Encoder[LogE] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeLog10: Decoder[Log10] =
    Decoder.forProduct1("args")(Log10.apply)
  implicit lazy val encodeLog10: Encoder[Log10] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeSquareRoot: Decoder[SquareRoot] =
    Decoder.forProduct1("args")(SquareRoot.apply)
  implicit lazy val encodeSquareRoot: Encoder[SquareRoot] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeAbs: Decoder[Abs] =
    Decoder.forProduct1("args")(Abs.apply)
  implicit lazy val encodeAbs: Encoder[Abs] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeDefined: Decoder[Defined] =
    Decoder.forProduct1("args")(Defined.apply)
  implicit lazy val encodeDefined: Encoder[Defined] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeUndefined: Decoder[Undefined] =
    Decoder.forProduct1("args")(Undefined.apply)
  implicit lazy val encodeUndefined: Encoder[Undefined] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeNumNeg: Decoder[NumericNegation] =
    Decoder.forProduct1("args")(NumericNegation.apply)
  implicit lazy val encodeNumNeg: Encoder[NumericNegation] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeLogicNeg: Decoder[LogicalNegation] =
    Decoder.forProduct1("args")(LogicalNegation.apply)
  implicit lazy val encodeLogicNeg: Encoder[LogicalNegation] =
    Encoder.forProduct2("args", "symbol")(u => (u.children, u.sym))

  implicit lazy val decodeImageSelect: Decoder[ImageSelect] =
    Decoder.forProduct2("args", "labels")(ImageSelect.apply)
  implicit lazy val encodeImageSelect: Encoder[ImageSelect] =
    Encoder.forProduct3("args", "labels", "symbol")(u => (u.children, u.labels, u.sym))

  implicit lazy val decodeIntLit: Decoder[IntLit] =
    Decoder.forProduct1("value")(IntLit.apply)
  implicit lazy val encodeIntLit: Encoder[IntLit] =
    Encoder.forProduct2("value", "symbol")(u => (u.value, u.sym))

  implicit lazy val decodeIntvar: Decoder[IntVar] =
    Decoder.forProduct1("name")(IntVar.apply)
  implicit lazy val encodeIntvar: Encoder[IntVar] =
    Encoder.forProduct2("name", "symbol")(u => (u.name, u.sym))

  implicit lazy val decodeDblLit: Decoder[DblLit] =
    Decoder.forProduct1("value")(DblLit.apply)
  implicit lazy val encodeDblLit: Encoder[DblLit] =
    Encoder.forProduct2("value", "symbol")(u => (u.value, u.sym))

  implicit lazy val decodeDblVar: Decoder[DblVar] =
    Decoder.forProduct1("name")(DblVar.apply)
  implicit lazy val encodeDblVar: Encoder[DblVar] =
    Encoder.forProduct2("name", "symbol")(u => (u.name, u.sym))

  implicit lazy val decodeBoolLit: Decoder[BoolLit] =
    Decoder.forProduct1("value")(BoolLit.apply)
  implicit lazy val encodeBoolLit: Encoder[BoolLit] =
    Encoder.forProduct2("value", "symbol")(u => (u.value, u.sym))

  implicit lazy val decodeBoolVar: Decoder[BoolVar] =
    Decoder.forProduct1("name")(BoolVar.apply)
  implicit lazy val encodeBoolVar: Encoder[BoolVar] =
    Encoder.forProduct2("name", "symbol")(u => (u.name, u.sym))

  implicit lazy val decodeGeomLit: Decoder[GeomLit] =
    Decoder.forProduct1("geom")(GeomLit.apply)
  implicit lazy val encodeGeomLit: Encoder[GeomLit] =
    Encoder.forProduct2("geom", "symbol")(u => (u.geom, u.sym))

  implicit lazy val decodeGeomVar: Decoder[GeomVar] =
    Decoder.forProduct1("name")(GeomVar.apply)
  implicit lazy val encodeGeomVar: Encoder[GeomVar] =
    Encoder.forProduct2("name", "symbol")(u => (u.name, u.sym))

  implicit lazy val decodeRasterVar: Decoder[RasterVar] =
    Decoder.forProduct1("name")(RasterVar.apply)
  implicit lazy val encodeRasterVar: Encoder[RasterVar] =
    Encoder.forProduct2("name", "symbol")(u => (u.name, u.sym))
}
