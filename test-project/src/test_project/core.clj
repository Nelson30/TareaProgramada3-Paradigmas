(ns test-project.core
  (:gen-class))


(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn hacer-hash [s]
  (reduce conj (map hash-map [:x :y] (.split s ","))))

(defn obtener-xs "obtiene las x"
  [vals-map]
  (do
    (for [r vals-map]
      (Integer/parseInt (get-in r [:x])))
  )
 )

(defn obtener-ys "obtiene las y"
  [vals-map]
  (do
    (for [r vals-map]
      (Integer/parseInt (get-in r [:y])))
    )
  )

(defn suma "suma cualquier lista"
  [lista-numeros]
  (reduce + lista-numeros)
  )

(defn obtener-promedio "obtiene la media de una lista"
  [lista]
  (/ (suma lista) (count lista))
  )

(defn obtener-factoresx "obtiene los factores de x"
  [lista-xs]
  (do

    (for [r lista-xs]
      (- r (obtener-promedio lista-xs))
    )
  )
)

(defn obtener-factoresy "obtiene los factores de Y"
  [lista-ys]
  (do
    (for [r lista-ys]
      (- r (obtener-promedio lista-ys))
      )
    )
  )

(defn multiplicar-listas "multiplica listas"
  [factores-x factores-y]
  (map * factores-x factores-y)
 )

(defn obtener-fac "obtiene número a ser divido por el total de elementos"
  [valores]
  (multiplicar-listas (obtener-factoresx (obtener-xs valores)) (obtener-factoresy (obtener-ys valores)))
)


(defn obtener-covarianza "realiza el cálculo para obtener covarianza o numerador"
  [valores]
  (/ (reduce + (obtener-fac valores)) (count valores))
)

(defn obtener-factxden "obtiene los factores de x del denominador elevados al cuadrado"
  [valores]
  (/ (reduce + (multiplicar-listas (obtener-factoresx (obtener-xs valores)) (obtener-factoresx (obtener-xs valores)))) (count valores))
)

(defn obtener-factyden "obtiene los factores de y del denominador elevados al cuadrado"
  [valores]
  (/ (reduce + (multiplicar-listas (obtener-factoresy (obtener-ys valores)) (obtener-factoresy (obtener-ys valores)))) (count valores))
 )


(defn obtener-denominadorsinroot "obtiene denominador sin raiz cuadrado"
  [valores]
  (* (obtener-factxden valores) (obtener-factyden valores))
)

(defn obtener-denominador "obtiene denominador"
  [valores]
  (Math/sqrt (obtener-denominadorsinroot valores))
 )


(defn obtener-correlacion "realiza el cálculo de correlacion"
  [valores]
  (/ (obtener-covarianza valores) (obtener-denominador valores))
 )

(defn imprimir-correlacion "Dice al usuario el nivel de correlacion"
  [valores]
  (do
  (def correlacion (obtener-correlacion valores))
  (cond
    (>= correlacion 1) (println correlacion "indica -> Correlación Fuerte")
    (< correlacion 1) (println correlacion "indica -> Correlación débil")
    (= correlacion 0) (println correlacion "indica -> No hay Correlación")
    :else (println "Ah sí? Ven aquí!"))
 )
)

(defn -main
  "Funcion principal"
  [& args]
  (do
    (def valores (map hacer-hash (read-lines "/Users/nelsonalfaro/Documents/ParadigmasDeProgramacion/clojure/test_project/test-project/resources/test.txt")))
    (println valores)
    (imprimir-correlacion valores)
  )
 )


