import scala.collection.mutable.HashMap

object EntryPoint {
	val swarmSize = 20
	val maxIterations = 1000
	val dimensions = 20
	val functionIndex = 3
	val maxRuns = 30
	var swarm = new Array[Particle](swarmSize)
	var counter = 0

	def main(args: Array[String]): Unit = {
		printf("Swarm of size %d created.\n", swarmSize)
		printf("Number of dimensions: %d.\n", dimensions)
		printf("Using function %d.\n", functionIndex)
		search()
	}

	def search() = {
		var bestParameters = new HashMap[String, Double]()
		var bestParticle = new HashMap[String, Double]()
		var currentResults = new Array[Double](maxRuns)
		var currentOptimumResults = new Array[Double](maxRuns)
		bestParameters += ("optimum" -> Double.MaxValue, "w" -> Double.MaxValue, "c1" -> Double.MaxValue, "c2" -> Double.MaxValue)

		// brute force w and c1&c2
		for (w <- -1.1 to 1.1 by 0.1; c1 <- 0.1 to 2.5 by 0.1) {
			var currIteration = 0
			var waitTime = 100
			var c2 = c1
			var sumOfResults = 0.0

			if(bestParameters("optimum") != Double.MaxValue){
				// printf("\rBest configuration after %d%% done: %s                           ", counter * 100 / 574, bestParameters.toString())
				printf("\rProgress: %d%%                   ", counter * 100 / 574)
			}
			else {
				printf("\rRunning first iteration...")
			}

			counter += 1

			// run each configuration of w and c1&c2 maxRuns times.
			for(currentRun <- 1 to maxRuns) {
				// printf("\rCurrent average for run %d is %f.           ", currentRun, sumOfResults / currentRun)
				// initialize swarm
				for (i <- 0 to swarmSize - 1) {
					swarm(i) = new Particle
					
					if(swarm(i).getW() != w){
						swarm(i).setW(w)
					}

					if(swarm(i).getC1() != c1){
						swarm(i).setC1(c1)
					}

					if(swarm(i).getC2() != c2){
						swarm(i).setC2(c2)
					}
				}

				// run PSO maxIterations iterations
				while(currIteration < maxIterations) {
					// printf("\rCurrent iteration %d out of max %d. Best solution is %f. Max Velocity: %.4f               ", currIteration + 1, maxIterations, Particle.getNeighbourhoodBest()("result"), Particle.getMaxVelocity())
					// Thread.sleep(waitTime)
					
					// for each particle in swarm
					for(particle <- swarm) {
						var objectiveFunctionResult = Particle.objectiveFunction(particle.getCurrentPosition()) // get "fitness"

						if(objectiveFunctionResult("result") < particle.getPersonalBest()("result")) // if current pos is better than particle's personal best then update it
							particle.setPersonalBest(objectiveFunctionResult)

						if(particle.getPersonalBest()("result") < Particle.getNeighbourhoodBest()("result")) // if current pos is better than particle's neighbourhood's best then update it
							Particle.setNeighbourhoodBest(particle.getPersonalBest(), currIteration)
					}

					// for each particle in swarm
					for(particle <- swarm) {
						particle.updateVelocity()
						particle.updatePosition()
					}

					// Particle.updateMaxVelocity(currIteration, maxIterations)
					currIteration += 1
				}

				// printf("\rBest solution found was: %.6f. With values:                                                        \n", Particle.getNeighbourhoodBest()("result"))
				// printf("%s\n",Particle.getNeighbourhoodBest().toString())
				sumOfResults += Particle.getNeighbourhoodBest()("result")
				currentResults(currentRun - 1) = Particle.getNeighbourhoodBest()("result")
			}

			sumOfResults /= maxRuns

			if(sumOfResults < bestParameters("optimum")){
				bestParameters("optimum") = sumOfResults
				bestParameters("w") = w
				bestParameters("c1") = c1
				bestParameters("c2") = c2
				bestParticle = Particle.getNeighbourhoodBest()
				currentOptimumResults = currentResults
			}
		}

		// println() // just print new line
		var variance = 0.0
		for(i <- 0 to maxRuns - 1) {
			variance += Math.pow(currentOptimumResults(i) - bestParticle("result"), 2)
		}
		variance /= maxRuns
		printf("\nBest solution found was:\n")
		printf("\tw:  %f\n", bestParameters("w"))
		printf("\tc1:  %f\n", bestParameters("w"))
		printf("\tc2:  %f\n", bestParameters("w"))
		printf("\tAverage:  %f\n", bestParticle("result"))
		printf("\tVariance: %f\n", variance)
		printf("With values:\n")
		for(i <- 0 to dimensions - 1) {
			printf("\tx%d=%f\n", i, bestParticle("x" + i))
		}
	}

	class Particle(var c1: Double, var c2: Double, var w: Double) {
		val id = Particle.newId
		var currentVelocity = Particle.initializeVelocity()
		var currentPosition = Particle.initializePosition()
		var personalBest = Particle.objectiveFunction(currentPosition)
		
		init

		def this() = {
			this(0.5, 0.5, 0.7)
		}

		def init(): Unit = {
			this.setC1(c1)
			this.setC2(c2)
			this.setW(w)

			if(id == 0) {
				Particle.setNeighbourhoodBest(this.personalBest, 0)
			}
		}

		def getId() : Int = id
		def getC1() : Double = c1
		def getC2() : Double = c2
		def getW()  : Double = w
		def getCurrentVelocity() : HashMap[String, Double] = currentVelocity
		def getCurrentPosition() : HashMap[String, Double] = currentPosition
		def getPersonalBest() : HashMap[String, Double] = personalBest

		def setC1(c1 : Double) : Unit = {
			this.c1 = c1
		}

		def setC2(c2 : Double) : Unit = {
			this.c2 = c2
		}

		def setW(w : Double) : Unit = {
			this.w = w
		}

		def setCurrentVelocity(currentVelocity : HashMap[String, Double]) : Unit = {
			this.currentVelocity = currentVelocity
		}

		def setCurrentPosition(currentPosition : HashMap[String, Double]) : Unit = {
			this.currentPosition = currentPosition
		}

		def setPersonalBest(personalBest : HashMap[String, Double]) : Unit = {
			this.personalBest = personalBest
		}

		def updateVelocity() : Unit = {
			this.currentVelocity = Particle.compunentWiseVectorAddition(Particle.scalarTimesVector(w, this.currentVelocity), Particle.compunentWiseVectorAddition(cognitiveWeight(), socialWeight()))

			// this.currentVelocity.foreach((keyvalue) => {
			// 	if(Math.abs(keyvalue._2) > Particle.maxVelocity){
			// 		this.currentVelocity(keyvalue._1) = Particle.maxVelocity * (keyvalue._2 / Math.abs(keyvalue._2))
			// 	}
			// })
		}

		def cognitiveWeight() : HashMap[String, Double] = {
			val r1 = Particle.getRandomVector(currentPosition)
			return Particle.compunentWiseVectorMultiplication(Particle.scalarTimesVector(c1, r1), Particle.compunentWiseVectorSubtraction(personalBest, currentPosition))
		}

		def socialWeight() : HashMap[String, Double] = {
			val r2 = Particle.getRandomVector(currentPosition)
			return Particle.compunentWiseVectorMultiplication(Particle.scalarTimesVector(c2, r2), Particle.compunentWiseVectorSubtraction(Particle.getNeighbourhoodBest(), currentPosition))
		}

		def updatePosition() : Unit = {
			this.currentPosition = Particle.compunentWiseVectorAddition(currentPosition, currentVelocity)
		}

		override def toString() : String = {
			return "Particle {" +
				"\tid: " + this.id + ",\n" +
				"\tc1: " + this.c1 + ",\n" +
				"\tc2: " + this.c2 + ",\n" +
				"\tw: " + this.w + "\n" +
			"}"
		}
	}

	object Particle {
		var random = scala.util.Random
		var maxInitialVelocity = 5.0
		var maxVelocity = maxInitialVelocity
		var idCounter = 0
		var neighbourhoodBest = new HashMap[String, Double]()
		var neighbourhoodBestIteration = 0

		def newId() : Int = {
			idCounter += 1;
			return idCounter - 1;
		}


		def getMaxVelocity() : Double = maxVelocity

		def getNeighbourhoodBest() : HashMap[String, Double] = neighbourhoodBest

		def setNeighbourhoodBest(neighbourhoodBest : HashMap[String, Double], iterationFound: Int) : Unit = {
			this.neighbourhoodBest = neighbourhoodBest
			this.neighbourhoodBestIteration = iterationFound
		}

		def compareAndSetNeighbourhoodBest(neighbourhoodBest : HashMap[String, Double]) : Boolean = {
			if(this.neighbourhoodBest("result") > neighbourhoodBest("result")){
				this.neighbourhoodBest = neighbourhoodBest
				return true
			}
			else
				return false
		}

		def initializePosition() : HashMap[String,Double] = {
			var result = new HashMap[String, Double]()
			var range = new HashMap[String, Double]

			Main.functionIndex match {
				case 0 => {range = sphericalInitialPosition()}
				case 1 => {range = ackleyInitialPosition()}
				case 2 => {range = michalewiczInitialPosition()}
				case 3 => {range = katsuuraInitialPosition()}
			}

			for(d <- 0 to dimensions - 1) {
				result("x" + d) = random.nextDouble() * range("range") + range("min")
			}

			return result
		}

		def sphericalInitialPosition(): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			result += ("min" -> -5.12, "max" -> 5.12, "range" -> 10.24)
			return result
		}

		def ackleyInitialPosition(): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			result += ("min" -> -32.768, "max" -> 32.768, "range" -> 65.538)
			return result
		}

		def michalewiczInitialPosition(): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			result += ("min" -> 0, "max" -> scala.math.Pi, "range" -> scala.math.Pi)
			return result
		}

		def katsuuraInitialPosition(): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			result += ("min" -> -20, "max" -> 20, "range" -> 4)
			return result
		}

		def initializeVelocity() : HashMap[String,Double] = {
			var result = new HashMap[String, Double]()
			var range = new HashMap[String, Double]
			
			Main.functionIndex match {
				case 0 => {range = sphericalInitialPosition()}
				case 1 => {range = ackleyInitialPosition()}
				case 2 => {range = michalewiczInitialPosition()}
				case 3 => {range = katsuuraInitialPosition()}
			}

			for(d <- 0 to dimensions - 1) {
				val r = random.nextDouble()
				result("x" + d) = r * (range("range") * 0.1)

				if(random.nextDouble() > 0.5){
					result("x" + d) *= -1.0
				}
			}

			return result
		}

		def updateMaxVelocity(currIteration: Int, maxIterations: Int) : Unit = {
			this.maxVelocity = (1.0 - Math.pow(1.0 * currIteration / maxIterations, 0.5)) * this.maxInitialVelocity
		}


		def objectiveFunction(position : HashMap[String, Double]) : HashMap[String, Double] = {
			Main.functionIndex match {
				case 0 => return sphericalObjectiveFunction(position)
				case 1 => return ackleyObjectiveFunction(position)
				case 2 => return michalewiczObjectiveFunction(position)
				case 3 => return katsuuraObjectiveFunction(position)
			}
			
			return HashMap[String, Double]()
		}

		def sphericalObjectiveFunction(position : HashMap[String, Double]): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			var sum = 0.0
			position.foreach((keyvalue) => {
				sum += keyvalue._2 * keyvalue._2
				result(keyvalue._1) = keyvalue._2
			})
			result("result") = sum
			return result
		}

		def ackleyObjectiveFunction(position : HashMap[String, Double]): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			var sum1 = 0.0
			var sum2 = 0.0

			position.foreach((keyvalue) => {
				sum1 += keyvalue._2 * keyvalue._2
				sum2 += Math.cos(2 * scala.math.Pi * keyvalue._2)
				result(keyvalue._1) = keyvalue._2
			})

			result("result") = -20.0 * Math.exp(-0.2 * Math.sqrt(sum1 / dimensions)) - Math.exp(sum2 / dimensions) + 20 + Math.exp(1)
			return result
		}

		def michalewiczObjectiveFunction(position : HashMap[String, Double]): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			var sum = 0.0
			var m = 10

			position.foreach((keyvalue) => {
				val xindex = keyvalue._1.substring(1).toInt + 1
				sum += Math.sin(keyvalue._2) * Math.pow(Math.sin(xindex * Math.pow(keyvalue._2, 2) / scala.math.Pi), 2 * m) 
				result(keyvalue._1) = keyvalue._2
			})

			result("result") = -sum
			return result
		}

		def katsuuraObjectiveFunction(position : HashMap[String, Double]): HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			var product = 1.0
			var tenDivDTwoOne = 10 / Math.pow(Main.dimensions, 1.2)
			val tenDivDSquare = 10 / Math.pow(Main.dimensions, 2)

			position.foreach((keyvalue) => {
				var sum = 0.0
				val xindex = keyvalue._1.substring(1).toInt + 1

				for(i <- 1 to 32) {
					sum += Math.abs(Math.pow(2.0, i) * keyvalue._2 - Math.round(Math.pow(2.0, i) * keyvalue._2)) / Math.pow(2.0, i)
					result(keyvalue._1) = keyvalue._2
				}

				product *= Math.pow((1 + xindex * sum), tenDivDTwoOne)
				result(keyvalue._1) = keyvalue._2
			})

			result("result") = tenDivDSquare * product - tenDivDSquare
			return result
		}

		def getRandomVector(vectorTemplate: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			vectorTemplate.foreach((keyvalue) => result(keyvalue._1) = random.nextDouble())
			return result
		}

		def scalarTimesVector(s: Double, v: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v.foreach((keyvalue) => result(keyvalue._1) = keyvalue._2 * s)
			return result
		}

		def compunentWiseVectorAddition(v1: HashMap[String, Double], v2: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v1.foreach((keyvalue) => {
				if(v2.contains(keyvalue._1))
					result(keyvalue._1) = keyvalue._2 + v2(keyvalue._1)	
			})
			return result
		}

		def compunentWiseVectorSubtraction(v1: HashMap[String, Double], v2: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v1.foreach((keyvalue) => {
				if(v2.contains(keyvalue._1))
					result(keyvalue._1) = keyvalue._2 - v2(keyvalue._1)	
			})
			return result
		}

		def compunentWiseVectorMultiplication(v1: HashMap[String, Double], v2: HashMap[String, Double]) : HashMap[String, Double] = {
			var result = new HashMap[String, Double]()
			v1.foreach((keyvalue) => {
				if(v2.contains(keyvalue._1))
					result(keyvalue._1) = keyvalue._2 * v2(keyvalue._1)	
			})
			return result
		}
	}
}
