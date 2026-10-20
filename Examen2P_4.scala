abstract class Servicio(val nombre: String, val horas: Int) {
    def calcularCosto(): Double
    def descripcion(): String = {
        return "Servicio de " + nombre + " por " + horas + " horas"
    }
}

trait Descontable {
    def porcentaje: Double
    def conDescuento(costo: Double): Double = {
        val descuento = costo * porcentaje / 100
        return costo - descuento
    }
}

class Catering(horas: Int, val personas: Int, val precioPersona: Double)
    extends Servicio("Catering", horas) with Descontable {
    def porcentaje: Double = {
        return 10
    }
    def calcularCosto(): Double = {
        return personas * precioPersona
    }
}

class Fotografia(horas: Int, val precioHora: Double, val albumes: Int)
    extends Servicio("Fotografia", horas) {
    def calcularCosto(): Double = {
        val costoHoras = horas * precioHora
        val costoAlbumes = albumes * 50
        return costoHoras + costoAlbumes
    }
}

class Sonido(horas: Int, val precioHora: Double, val conDJ: Boolean)
    extends Servicio("Sonido", horas) with Descontable {
    def porcentaje: Double = {
        return 5
    }
    def calcularCosto(): Double = {
        val costoHoras = horas * precioHora
        if (conDJ == true) {
            return costoHoras + 300
        } else {
            return costoHoras
        }
    }

    override def descripcion(): String = {
        if (conDJ == true) {
            return "Servicio de sonido con DJ por " + horas + " horas"
        } else {
            return "Servicio de sonido sin DJ por " + horas + " horas"
        }
    }
}

object Catering {
    def crear(horas: Int, personas: Int, precioPersona: Double): Catering = {
        require(horas > 0)
        require(personas > 0)
        require(precioPersona > 0)

        return new Catering(horas, personas, precioPersona)
    }
}

object Fotografia {
    def crear(horas: Int, precioHora: Double, albumes: Int): Fotografia = {
        require(horas > 0)
        require(precioHora > 0)
        require(albumes >= 0)

        return new Fotografia(horas, precioHora, albumes)
    }
}

object Sonido {
    def crear(horas: Int, precioHora: Double, conDJ: Boolean): Sonido = {
        require(horas > 0)
        require(precioHora > 0)

        return new Sonido(horas, precioHora, conDJ)
    }
}

class Paquete(val servicios: List[Servicio]) {
    def agregar(servicio: Servicio): Paquete = {
        val nuevaLista = servicio :: servicios
       
        return new Paquete(nuevaLista)
    }

    def total(): Double = {
        var resultado = 0.0
        for (s <- servicios) {
            resultado = resultado + s.calcularCosto()
        }
        return resultado
    }
}

object Examen2P_4 {
    def main(args: Array[String]): Unit = {

        val catering = Catering.crear(4, 50, 40)
        val foto = Fotografia.crear(5, 100, 2)
        val sonido = Sonido.crear(6, 80, true)

        println(catering.descripcion())
        println(catering.calcularCosto())

        println(foto.descripcion())
        println(foto.calcularCosto())

        println(sonido.descripcion())
        println(sonido.calcularCosto())

        val descuentoCatering = catering.conDescuento(catering.calcularCosto())
        val descuentoSonido = sonido.conDescuento(sonido.calcularCosto())
        println(descuentoCatering)
        println(descuentoSonido)

        val paquete1 = new Paquete(List(catering))
        val paquete2 = paquete1.agregar(foto)
        val paquete3 = paquete2.agregar(sonido)

        println(paquete1.total())
        println(paquete2.total())
        println(paquete3.total())

        println(paquete1.servicios.length)
    }
}