package object ConjuntosF {

  type Conj = Int => Boolean

  def pertenece(elem: Int, s: Conj) : Boolean = s(elem)

  /**
   * Ejercicio 1
   */

  def conjuntoUnitario(elem: Int) : Conj =
  {
    def verificarPertenencia(elemento: Int) : Boolean =
    {
      elemento == elem
    }
    verificarPertenencia
  }

  /**
   * Ejercicio 2
   */

  def union(conj1: Conj, conj2: Conj): Conj =
  {
    def unirConjuntos(elemento: Int): Boolean =
      {
        conj1(elemento) || conj2(elemento)
      }
    unirConjuntos
  }

  def intersect(conj1: Conj, conj2: Conj): Conj =
  {
    def interceptarConjuntos(elemento: Int): Boolean =
      {
        conj1(elemento) && conj2(elemento)
      }
    interceptarConjuntos
  }

  def dif(conj1: Conj , conj2: Conj ): Conj =
  {
    def restarConjuntos(elemento: Int):Boolean =
      {
        conj1(elemento) && !conj2(elemento)
      }
    restarConjuntos
  }

  /**
   * Ejercicio 3
   */

  def filtrar (conj : Conj, predicado : Int => Boolean) : Conj = {
    def filtrarElementos(elemento: Int):Boolean =
      {
        conj(elemento) && predicado(elemento)
      }
    filtrarElementos
  }

  /**
   * Ejercicio 4
   */

  def forall ( conj: Conj , predicado : Int => Boolean ) : Boolean = {
    def evaluarForall(elemento:Int):Boolean={
      if(elemento>1000)
        {
          true
        }
      else if(pertenece(elemento, conj) && !pertenece(elemento,predicado))
        {
          false
        }
      else
        {
          evaluarForall(elemento+1)
        }
    }
    evaluarForall(-1000)
  }

  /**
   * Ejercicio 5
   */

  def exists (conj: Conj , predicado : Int => Boolean ) : Boolean = {
    //!forall(conjunto,(elemento:Int)=> !pertenece(elemento,predicado))
    def determinarExistencia(elemento:Int):Boolean={
      if(elemento>1000)
        {
          false
        }
      else if(pertenece(elemento,conj) && pertenece(elemento,predicado))
        {
          true
        }
      else
        {
          determinarExistencia(elemento+1)
        }
    }
    determinarExistencia(-1000)
  }

  /**
   * Ejercicio 6
   */

  def map( conj: Conj , f: Int => Int ) : Conj = {
    def mapear(elemento: Int): Boolean =
      {
        exists(conj, (valor:Int) => f(valor)==elemento)
      }
    mapear
  }
}
