package br.com.caelum.fj16.argentum

import java.util.Calendar
import java.util.Calendar._
import java.text.SimpleDateFormat

class Calendario(calendar: Calendar) extends Ordered[Calendario] {
  import Calendario._

  private val _calendario: Calendar = calendar.clone.asInstanceOf[Calendar]

  def calendario: Calendar = _calendario.clone.asInstanceOf[Calendar]

  override def toString = formatter.format(_calendario.getTime)

  def compare(that: Calendario) = this._calendario.compareTo(that._calendario)

}

object Calendario {
  private[argentum] val formatter = new SimpleDateFormat("dd/MM/yyyy")

  implicit def calendarToCalendario(calendar: Calendar): Calendario = {
    require(calendar != null)
    new Calendario(calendar)
  }

  implicit def calendarioToCalendar(c: Calendario): Calendar = c.calendario

  def dia(n: Negocio) = (n.data.get(DAY_OF_MONTH), n.data.get(MONTH), n.data.get(YEAR))

}

class Negocio(val preco: Double, val quantidade: Int, val data: Calendario) {
  require(preco >= 0.0)
  require(quantidade > 0)
  require(data != null)

  def volume = preco * quantidade
  override def toString = data.toString
 
}

class Candlestick(val abertura: Double, val fechamento: Double, val minimo: Double, val maximo: Double, val volume: Double, val data: Calendario) {
  require(abertura >= 0.0)
  require(fechamento >= 0.0)
  require(minimo >= 0.0)
  require(maximo >= 0.0)
  require(volume >= 0.0)
  require(data != null)

  def alta = abertura < fechamento
  def baixa = !alta
  override def toString = "(abertura %f, fechamento %f, minimo %f, maximo %f, volume %f, data: %s)" format (abertura, fechamento, minimo, maximo, volume, data)
}

object Candlestick {
  import Calendario._
  def candlestick(negocios: List[Negocio]): Candlestick =
    negocios match {
      case Nil => new Candlestick(abertura = 0.0, fechamento = 0.0, minimo = 0.0, maximo = 0.0, volume = 0.0, Calendar.getInstance)
      case lista =>
        val maiorPreco = lista.map(_.preco).max
        val menorPreco = lista.map(_.preco).min
        val volume = lista.map(_.volume).sum
        new Candlestick(
          abertura = lista.head.preco,
          fechamento = lista.last.preco,
          minimo = menorPreco,
          maximo = maiorPreco,
          volume = volume,
          data = lista.head.data)
    }

  def candlesticks(negocios: List[Negocio]): List[Candlestick] = {
    val candlesticks = negocios.groupBy(dia).map {
      case (_, negociosNoDia) => candlestick(negociosNoDia)
    }.toList
    candlesticks.sortBy(_.data)
  }

}

