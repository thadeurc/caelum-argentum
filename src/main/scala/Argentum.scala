package br.com.caelum.fj16.argentum

import java.util.Calendar
import java.util.Calendar._
import java.text.SimpleDateFormat

class Calendario(calendar: Calendar) {
  import Calendario._

  private val _calendario:Calendar = calendar.clone.asInstanceOf[Calendar]

  def calendario:Calendar = _calendario.clone.asInstanceOf[Calendar]

  def mesmoDia(calendario: Calendario): Boolean = Calendario.mesmoDia(this, calendario)

  override def toString = formatter.format(_calendario.getTime)
  
}

object Calendario {
  private[argentum] val formatter = new SimpleDateFormat("dd/MM/yyyy")

  implicit def calendarToCalendario(calendar: Calendar): Calendario = {
    require(calendar != null)
    new Calendario(calendar)
  }
  
  implicit def calendarioToCalendar(c: Calendario): Calendar = c.calendario

  def mesmoDia(calendario1: Calendario, calendario2: Calendario): Boolean = {
    calendario1.get(DAY_OF_MONTH) == calendario2.get(DAY_OF_MONTH) &&
    calendario1.get(MONTH) == calendario2.get(MONTH) &&
    calendario1.get(YEAR) == calendario2.get(YEAR)
  }

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
}

object Candlestick {
  import Calendario._
  import ArgentumUtil._
  def candlestick(negocios: List[Negocio]): Candlestick =
    negocios match {
      case Nil => new Candlestick(abertura = 0.0, fechamento = 0.0, minimo = 0.0, maximo = 0.0, volume = 0.0, Calendar.getInstance)
      case lista =>
        val maior = lista.reduceLeft((n1, n2) => (if (n1.preco >= n2.preco) n1 else n2))
        val menor = lista.reduceLeft((n1, n2) => (if (n1.preco >= n2.preco) n2 else n1))
        val volume = lista.foldLeft(0.0)((acc: Double, n: Negocio) => (acc + n.volume))
        new Candlestick(
          abertura = lista.head.preco,
          fechamento = lista.last.preco,
          minimo = menor.preco,
          maximo = maior.preco,
          volume = volume,
          data = lista.head.data)
    }

  def candlesticks(negocios: List[Negocio]): List[Candlestick] = {
    agrupa(negocios, 
	   (n1:Negocio, n2:Negocio) => mesmoDia(n1.data, n2.data)
	 ).map(candlestick)
  }

}

object ArgentumUtil {
  def agrupa[A](lista: List[A], condicao: (A, A) => Boolean): List[List[A]] = {
    def _agrupa[A](lista: List[A], condicao: (A, A) => Boolean, acc: List[List[A]]): List[List[A]] =
      lista match {
        case List() => acc
        case cabeca :: cauda =>
          val (grupo, restante) = lista.span(candidato => condicao(cabeca, candidato))
          _agrupa(restante, condicao, grupo :: acc)
      }
    _agrupa(lista, condicao, List[List[A]]()).reverse
  }
}
