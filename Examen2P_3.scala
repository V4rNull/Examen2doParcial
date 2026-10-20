object Examen2P_3{
    def procesarRango(
        desde: Int,
        hasta: Int,
        transformar: Int => Int,
        combinar: (Int, Int) => Int,
        valorInicial: Int
    ): Int = {
        if desde == hasta then combinar(valorInicial, transformar(desde))
        else {
            val transformado = transformar(desde)
            val resultadoSiguienteRango = procesarRango(desde + 1, hasta, transformar, combinar, valorInicial)
            combinar(transformado, resultadoSiguienteRango)
        }
    }

    def buscar(
        desde: Int,
        hasta: Int,
        condicion: Int => Boolean
    ): Int = {
        if desde > hasta then -1
        else if condicion(desde) then desde
        else buscar(desde + 1, hasta, condicion)
    }

    def contarDesde(n: Int, d: Int): Int = {
        if d > n then 0
        else if n % d == 0 then 1 + contarDesde(n, d + 1)
        else contarDesde(n, d + 1)
    }

    def contarDivisores(n: Int): Int = {
        contarDesde(n, 1)
    }

    def maximo(a: Int, b: Int): Int = {
        if a > b then a else b
    }

    def esPrimo(n: Int): Boolean = {
        contarDivisores(n) == 2
    }

    def main(args: Array[String]): Unit = {
        println(procesarRango(22, 26, contarDivisores, maximo, 0))  
        println(procesarRango(5, 10, contarDivisores, maximo, 0))   
        println()
        println(buscar(6, 11, esPrimo))   
        println(buscar(24, 28, esPrimo))  
    }
}