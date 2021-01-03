all: build

build:
	sbt ';compile ;package'
	sbt ';test:compile ;test:package'

doc:
	sbt doc

genDistributedRouting:
    sbt 'test:runMain tetriski.purlin.utils.genDistributedRouting'

genSourceRouting:
    sbt 'test:runMain tetriski.purlin.utils.genSourceRouting'

genCircuitSwitched:
    sbt 'test:runMain tetriski.purlin.utils.genCircuitSwitched'

testInjectionRate:
    sbt 'test:runMain tetriski.purlin.utils.testInjectionRate'

testAlgorithms:
    sbt 'test:runMain tetriski.purlin.utils.testAlgorithms'

testNetworkCompare:
    sbt 'test:runMain tetriski.purlin.utils.testNetworkCompare'

clean:
	rm -f *.json *.fir *.v
	rm -rf test_run_dir/

cleanall: clean
	sbt clean