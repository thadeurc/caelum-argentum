import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import java.util._
import java.util.Calendar._
import br.com.caelum.fj16.argentum._
import br.com.caelum.fj16.argentum.Candlestick._
import br.com.caelum.fj16.argentum.Calendario._

class ArgentumSpec extends Spec with ShouldMatchers {

  describe("Uma Fabrica de Candlesticks") {
    val EPS = 1e-4
    it("Deve suportar uma lista de negocios nao vazia com MAIS de 1 elemento") {
      val hoje = Calendar.getInstance
      val negocios = new Negocio(40.5, 100, hoje) ::
        new Negocio(45.0, 100, hoje) ::
        new Negocio(39.8, 100, hoje) ::
        new Negocio(42.3, 100, hoje) ::
        Nil
      val candle = candlestick(negocios)

      candle.abertura should be(40.5 plusOrMinus EPS)
      candle.fechamento should be(42.3 plusOrMinus EPS)
      candle.minimo should be(39.8 plusOrMinus EPS)
      candle.maximo should be(45.0 plusOrMinus EPS)
      candle.volume should be(16760.0 plusOrMinus EPS)
    }

    it("Deve suportar uma lista de negocios VAZIA") {
      val EPS = 1e-4
      val hoje = Calendar.getInstance
      val negocios = Nil
      val candle = candlestick(negocios)

      candle.volume should be(0.0 plusOrMinus EPS)
    }

    it("Deve suportar uma lista de negocios nao vazia com APENAS de 1 elemento") {
      val EPS = 1e-4
      val hoje = Calendar.getInstance
      val negocios = new Negocio(40.5, 100, hoje) ::
        Nil
      val candle = candlestick(negocios)

      candle.abertura should be(40.5 plusOrMinus EPS)
      candle.fechamento should be(40.5 plusOrMinus EPS)
      candle.minimo should be(40.5 plusOrMinus EPS)
      candle.maximo should be(40.5 plusOrMinus EPS)
      candle.volume should be(4050.0 plusOrMinus EPS)
    }

    it("Deve saber criar candles com negocios do mesmo dia") {
      val hoje = getInstance
      val amanha = hoje.clone.asInstanceOf[Calendar]
      amanha.add(DAY_OF_MONTH, 1)
      val depois = amanha.clone.asInstanceOf[Calendar]
      depois.add(DAY_OF_MONTH, 1)

      val negocios = new Negocio(40.5, 100, hoje) ::
        new Negocio(45.0, 100, hoje) ::
        new Negocio(39.8, 100, hoje) ::
        new Negocio(42.3, 100, hoje) ::
        new Negocio(48.8, 100, amanha) ::
        new Negocio(49.3, 100, amanha) ::
        new Negocio(51.8, 100, depois) ::
        new Negocio(52.3, 100, depois) ::
        Nil
      val candles = candlesticks(negocios)
      candles.size should equal(3)
      println(candles)
      candles(0).abertura should be(40.5 plusOrMinus EPS)
      candles(0).fechamento should be(42.3 plusOrMinus EPS)
      candles(1).abertura should be(48.8 plusOrMinus EPS)
      candles(1).fechamento should be(49.3 plusOrMinus EPS)
      candles(2).abertura should be(51.8 plusOrMinus EPS)
      candles(2).fechamento should be(52.3 plusOrMinus EPS)
    }

  }

  describe("Um Negocio") {
    it("Deve ter sua data IMUTAVEL") {
      val data = Calendar.getInstance
      data.set(DAY_OF_MONTH, 15)
      val negocio = new Negocio(10.0, 5, data)
      negocio.data.set(DAY_OF_MONTH, 20)
      negocio.data.get(DAY_OF_MONTH) should equal(15)
    }

    it("Nao deve permitir data NULA") {
      intercept[IllegalArgumentException] {
        new Negocio(10.0, 5, null)
      }
    }
  }

  describe("Um Candlestick") {
    it("Nao deve permitir data NULA") {
      intercept[IllegalArgumentException] {
        new Candlestick(0.0, 0.0, 0.0, 0.0, 0, null)
      }
    }
  }
}
