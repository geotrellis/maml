package com.azavea.maml.ast.codec.tree

import com.azavea.maml.ast._
import com.azavea.maml.ast.codec._

import cats.syntax.functor._
import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException

trait ExpressionTreeCodec extends MamlCodecInstances {
  implicit lazy val totalEncoder: Encoder[Expression] = Encoder.instance {
    case il @ IntLit(_) => il.asJson
    case iv @ IntVar(_) => iv.asJson
    case gv @ GeomVar(_) => gv.asJson
    case gl @ GeomLit(_) => gl.asJson
    case dl @ DblLit(_) => dl.asJson
    case dv @ DblVar(_) => dv.asJson
    case bl @ BoolLit(_) => bl.asJson
    case bv @ BoolVar(_) => bv.asJson
    case rv @ RasterVar(_) => rv.asJson
    case rl @ RasterLit(_) =>
      throw new InvalidParameterException("Can't encode raster literal as JSON")
    case add @ Addition(_) => add.asJson
    case sub @ Subtraction(_) => sub.asJson
    case mul @ Multiplication(_) => mul.asJson
    case div @ Division(_) => div.asJson
    case max @ Max(_) => max.asJson
    case min @ Min(_) => min.asJson
    case mask @ Masking(_) => mask.asJson
    case cls @ Classification(_, _) => cls.asJson
    case fmax @ FocalMax(_, _, _) => fmax.asJson
    case fmin @ FocalMin(_, _, _) => fmin.asJson
    case fmean @ FocalMean(_, _, _) => fmean.asJson
    case fmed @ FocalMedian(_, _, _) => fmed.asJson
    case fmode @ FocalMode(_, _, _) => fmode.asJson
    case fsum @ FocalSum(_, _, _) => fsum.asJson
    case fstddev @ FocalStdDev(_, _, _) => fstddev.asJson
    case fslope @ FocalSlope(_, _, _) => fslope.asJson
    case fhillshade @ FocalHillshade(_, _, _, _, _) => fhillshade.asJson
    case faspect @ FocalAspect(_, _) => faspect.asJson
    case imgsel @ ImageSelect(_, _) => imgsel.asJson
    case lneg @ LogicalNegation(_) => lneg.asJson
    case nneg @ NumericNegation(_) => nneg.asJson
    case udfn @ Undefined(_) => udfn.asJson
    case dfn @ Defined(_) => dfn.asJson
    case abs @ Abs(_) => abs.asJson
    case sqrt @ SquareRoot(_) => sqrt.asJson
    case log10 @ Log10(_) => log10.asJson
    case loge @ LogE(_) => loge.asJson
    case ceil @ Ceil(_) => ceil.asJson
    case flr @ Floor(_) => flr.asJson
    case rnd @ Round(_) => rnd.asJson
    case acos @ Acos(_) => acos.asJson
    case asin @ Asin(_) => asin.asJson
    case tanh @ Tanh(_) => tanh.asJson
    case cosh @ Cosh(_) => cosh.asJson
    case sinh @ Sinh(_) => sinh.asJson
    case tan @ Tan(_) => tan.asJson
    case cos @ Cos(_) => cos.asJson
    case sin @ Sin(_) => sin.asJson
    case branch @ Branch(_) => branch.asJson
    case atan @ Atan(_) => atan.asJson
    case atan2 @ Atan2(_) => atan2.asJson
    case and @ And(_) => and.asJson
    case or @ Or(_) => or.asJson
    case xor @ Xor(_) => xor.asJson
    case gt @ Greater(_) => gt.asJson
    case gtoe @ GreaterOrEqual(_) => gtoe.asJson
    case equal @ Equal(_) => equal.asJson
    case unequal @ Unequal(_) => unequal.asJson
    case ltoe @ LesserOrEqual(_) => ltoe.asJson
    case lt @ Lesser(_) => lt.asJson
    case pow @ Pow(_) => pow.asJson
    case sleep @ Sleep(_, _) => sleep.asJson
  }

  implicit lazy val totalDecoder: Decoder[Expression] = Decoder.instance[Expression] { cursor =>
    cursor._symbol map {
      case "+" => Decoder[Addition]
      case "-" => Decoder[Subtraction]
      case "*" => Decoder[Multiplication]
      case "/" => Decoder[Division]
      case "max" => Decoder[Max]
      case "min" => Decoder[Min]
      case "mask" => Decoder[Masking]
      case "**" => Decoder[Pow]
      case "<" => Decoder[Lesser]
      case "<=" => Decoder[LesserOrEqual]
      case "!=" => Decoder[Unequal]
      case "=" => Decoder[Equal]
      case ">=" => Decoder[GreaterOrEqual]
      case ">" => Decoder[Greater]
      case "or" => Decoder[Or]
      case "xor" => Decoder[Xor]
      case "and" => Decoder[And]
      case "atan2" => Decoder[Atan2]
      case "ifelse" => Decoder[Branch]
      case "classify" => Decoder[Classification]
      case "sin" => Decoder[Sin]
      case "cos" => Decoder[Cos]
      case "tan" => Decoder[Tan]
      case "sinh" => Decoder[Sinh]
      case "cosh" => Decoder[Cosh]
      case "tanh" => Decoder[Tanh]
      case "asin" => Decoder[Asin]
      case "acos" => Decoder[Acos]
      case "atan" => Decoder[Atan]
      case "round" => Decoder[Round]
      case "floor" => Decoder[Floor]
      case "Ceil" => Decoder[Ceil]
      case "loge" => Decoder[LogE]
      case "log10" => Decoder[Log10]
      case "sqrt" => Decoder[SquareRoot]
      case "abs" => Decoder[Abs]
      case "def" => Decoder[Defined]
      case "undef" => Decoder[Undefined]
      case "nneg" => Decoder[NumericNegation]
      case "lneg" => Decoder[LogicalNegation]
      case "fmax" => Decoder[FocalMax]
      case "fmin" => Decoder[FocalMin]
      case "fmean" => Decoder[FocalMean]
      case "fmedian" => Decoder[FocalMedian]
      case "fmode" => Decoder[FocalMode]
      case "fsum" => Decoder[FocalSum]
      case "fstddev" => Decoder[FocalStdDev]
      case "fslope" => Decoder[FocalSlope]
      case "fhillshade" => Decoder[FocalHillshade]
      case "faspect" => Decoder[FocalAspect]
      case "sel" => Decoder[ImageSelect]
      case "int" => Decoder[IntLit]
      case "intV" => Decoder[IntVar]
      case "dbl" => Decoder[DblLit]
      case "dblV" => Decoder[DblVar]
      case "bool" => Decoder[BoolLit]
      case "boolV" => Decoder[BoolVar]
      case "geom" => Decoder[GeomLit]
      case "geomV" => Decoder[GeomVar]
      case "rasterV" => Decoder[RasterVar]
      case "sleep" => Decoder[Sleep]
    } match {
      case Some(decoder) => decoder.widen(cursor)
      case None =>  Left(DecodingFailure(s"No symbol provided for MAML expression", cursor.history))
    }
  }
}

