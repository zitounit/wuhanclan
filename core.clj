(ns wuhanclan.core
    (:use amoebas.defs amoebas.lib amoebas.run amoebas.examples amoebas.util) 
   )

  
   (def Neighbor-To-Dir2  
{ 0 [-1 -1] 1 [0 -1] 2 [1 -1] 3 [1 0] 4 [1 1] 5 [0 1] 6 [-1 1] 7 [-1 0] } 
)

   (defn create-wuhaner
    [low-energy divide-energy select-target mutation-rate mutation-range]
    
    
    (fn [energy health species env data]
        (let
            [
                do-hit2  (fn []
                (let
                    [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                         ;; eat
                     {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                  )
            )
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    hs  (hostiles species Neighbors env)
                                    empty-nb     (empty-neighbors env)  
                                    by-fuel      (sections-by-fuel empty-nb env)
                                ]
                                
                                (if (empty? empty-nb)  
                                    (if (empty? hs)     ;; no empty neighbors?
                                    {:cmd :rest} ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    (do-hit2)
                                    )
                                    (if (< (:fuel (env (Neighbor-To-Dir2 (last by-fuel)))) (:fuel (env Here))) 
                                        {:cmd :rest}
                                        {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel 
                                     )
                            )
                        )
                )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]
                                
                                (if (empty? hs)      ;; nobody to hit?
                                    (do-fuel)       ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            (if (<= (rand) mutation-rate)
                                {:cmd :divide :dir (rand-nth empty-nb) 
                                 :function 
                                    (create-mutating-slightlybrainy 
                                        (bound MoveEnergy (+ low-energy (rand-int (inc (* 2 mutation-range))) (- mutation-range)) MaxAmoebaEnergy)
                                        (bound MinDivideEnergy (+ divide-energy (rand-int (inc (* 2 mutation-range))) (- mutation-range)) MaxAmoebaEnergy)
                                        select-target 
                                        mutation-rate
                                        mutation-range) }
                                {:cmd :divide :dir (rand-nth empty-nb) }
                            )
                        )
            ]

            (cond
                (< energy low-energy)           ;; need some chow?
                    (do-fuel)
                (< divide-energy energy)               ;; parenthood!
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (hostiles species Neighbors env)            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em
                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)

;;
;;  This species tries to determine its distance from the edge, and goes into hinterland mode
;;  when it's far enough from the action.
;;
;;  :data fields: 
;;      :edge-distance
;;


     


(def Evam (create-wuhaner 10 35 most-energy-target-selector 0.1 1000))
;;
;;  A very simple example of random mutation. With some probability, this species modifies its
;;  own parameters when it divides.
;;
