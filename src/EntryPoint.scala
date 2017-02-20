import scala.collection.mutable.HashMap

object EntryPoint {
	val swarmSize = 20
	val maxIterations = 1000
	val dimensions = 20
	val functionIndex = 0
	var swarm = new Array[Particle](swarmSize)

	def main(args: Array[String]): Unit = {
		// Create swarm
		for (i <- 0 to swarmSize - 1) {
			swarm(i) = new Particle
		}

		printf("Swarm of size %d created.\n", swarmSize)
		search()
	}

	def search() = {
		var currIteration = 0
		var foundSolution = false
		var waitTime = 10

		while(currIteration < maxIterations && !foundSolution) {
			printf("\rCurrent iteration %d out of max %d. Best solution is %.3f. Max Velocity: %.4f               ", currIteration + 1, maxIterations, Particle.getNeighbourhoodBest()("result"), Particle.getMaxVelocity())
			// Thread.sleep(waitTime)

			for(particle <- swarm) {
				var objectiveFunctionResult = Particle.objectiveFunction(particle.getCurrentPosition())

				if(objectiveFunctionResult("result") < particle.getPersonalBest()("result"))
					particle.setPersonalBest(objectiveFunctionResult)

				if(particle.getPersonalBest()("result") < Particle.getNeighbourhoodBest()("result"))
					Particle.setNeighbourhoodBest(particle.getPersonalBest(), currIteration)
			}

			for(particle <- swarm) {
				particle.updateVelocity()
				particle.updatePosition()
			}

			Particle.updateMaxVelocity(currIteration, maxIterations)
			currIteration += 1
		}

		printf("\rBest solution found was: %.6f. With values:                                                        \n", Particle.getNeighbourhoodBest()("result"))
		printf("%s\n",Particle.getNeighbourhoodBest().toString())
	}

	class Particle(var c1: Double, var c2: Double, var w: Double) {
		val id = Particle.newId
		var currentVelocity = Particle.initializeVelocity()
		var currentPosition = Particle.initializePosition()
		var personalBest = Particle.objectiveFunction(currentPosition)
		
		init

		def this() = {
			this(0.5, 0.5, 0.9)
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

			this.currentVelocity.foreach((keyvalue) => {
				if(Math.abs(keyvalue._2) > Particle.maxVelocity){
					this.currentVelocity(keyvalue._1) = Particle.maxVelocity * (keyvalue._2 / Math.abs(keyvalue._2))
				}
			})
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

			for(d <- 0 to dimensions - 1) {
				result("x" + d) = random.nextDouble() * 65.538 - 32.768
			}

			return result
		}

		def initializeVelocity() : HashMap[String,Double] = {
			var result = new HashMap[String, Double]()

			for(d <- 0 to dimensions - 1) {
				result("x" + d) = random.nextDouble() * this.maxInitialVelocity * 2 - this.maxInitialVelocity
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