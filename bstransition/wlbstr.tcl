source ../../lan/vlan.tcl
source ../../lan/ns-mac.tcl

set opt(chan)           Channel/WirelessChannel    ;# channel type
set opt(prop)           Propagation/TwoRayGround   ;# radio-propagation model
set opt(netif)          Phy/WirelessPhy            ;# network interface type
set opt(mac)            Mac/802_11                 ;# MAC type
set opt(ifq)            Queue/DropTail/PriQueue    ;# interface queue type
set opt(ll)             LL                         ;# link layer type
set opt(ant)            Antenna/OmniAntenna        ;# antenna model
set opt(ifqlen)         50                         ;# max packet in ifq
set opt(nn)             3                         ;# number of mobilenodes
set opt(adhocRouting)   DSDV                       ;# routing protocol

set opt(cp)             ""                         ;# connection pattern file
set opt(sc)     "scentest1"  ;# node movement file. 
set opt(delay) 1ms
set opt(x)      500                          ;# x coordinate of topology
set opt(y)      500                          ;# y coordinate of topology
set opt(seed)   0.0                          ;# seed for random number gen.
set opt(stop)   250                          ;# time to stop simulation
set opt(bw)   11Mb

set opt(ftp1-start)      50.0
set opt(ftp2-start)      50.0
set opt(ftp3-start)      50.0
set opt(ftp4-start)      50.0
set opt(ftp5-start)      50.0

set num_wired_nodes      2
set num_bs_nodes         2

Mac/802_11 set dataRate_ 11Mb

set f1 [open sn1.tr w]
set f2 [open sn2.tr w]
set f3 [open sn3.tr w]


if { $opt(x) == 0 || $opt(y) == 0 } {
	puts "No X-Y boundary values given for wireless topology\n"
}
if {$opt(seed) > 0} {
	puts "Seeding Random number generator with $opt(seed)\n"
	ns-random $opt(seed)
}

# create simulator instance
set ns_   [new Simulator]

# set up for hierarchical routing
$ns_ node-config -addressType hierarchical
AddrParams set domain_num_ 4           ;# number of domains
lappend cluster_num 1 1 1 1             ;# number of clusters in each domain
AddrParams set cluster_num_ $cluster_num
lappend eilastlevel 3 4 1 1           ;# number of nodes in each cluster 
AddrParams set nodes_num_ $eilastlevel ;# of each domain

set tracefd  [open snp-bs1.tr w] 
set namtrace [open snp-bs1.nam w]
$ns_ use-newtrace
$ns_ trace-all $tracefd
$ns_ namtrace-all-wireless $namtrace $opt(x) $opt(y)

# Create topography object
set topo   [new Topography]

# define topology
$topo load_flatgrid $opt(x) $opt(y)

# create God
create-god [expr $opt(nn) + $num_bs_nodes]

set W(0) [$ns_ node 0.0.1]
set W(2) [$ns_ node 2.0.0]

# configure for base-station node
$ns_ node-config -adhocRouting $opt(adhocRouting) \
                 -llType $opt(ll) \
                 -macType $opt(mac) \
                 -ifqType $opt(ifq) \
                 -ifqLen $opt(ifqlen) \
                 -antType $opt(ant) \
                 -propType $opt(prop) \
                 -phyType $opt(netif) \
                 -channelType $opt(chan) \
		 -topoInstance $topo \
                 -wiredRouting ON \
		 -agentTrace ON \
                 -routerTrace OFF \
                 -macTrace OFF \


lappend nodelist $W(0)

set lan [$ns_ newLan $nodelist 100Mb \
	$opt(delay) -llType LL -ifqType $opt(ifq) \
		-macType Mac/802_3 -chanType Channel -address "0.0.0"]

set opt(ifq) Queue/DropTail


set W(1) [$ns_ node 0.0.2]

$lan addNode [list $W(1)] 100Mb 1ms LL/LLSnoop Queue/DropTail Mac/802_3

#create base-station node
set temp {1.0.0 1.0.1 1.0.2 1.0.3 3.0.0 1.0.5}      
           
#Introducing error into the inbound and outbound connections of wireless nodes  
$ns_ node-config -IncomingErrProc UniformErr \
                 -OutgoingErrProc UniformErr

proc UniformErr {} {
   set err [new ErrorModel]
   $err unit packet
   return $err
}

set BS(0) [$ns_ node [lindex $temp 0]]
$BS(0) random-motion 0               ;# disable random motion

#provide some co-ord (fixed) to base station node
$BS(0) set X_ 200.000000
$BS(0) set Y_ 290.000000
$BS(0) set Z_ 0.000000


set BS(1) [$ns_ node [lindex $temp 4]]
$BS(1) random-motion 0               ;# disable random motion

#provide some co-ord (fixed) to base station node
$BS(1) set X_ 215.000000
$BS(1) set Y_ 350.000000
$BS(1) set Z_ 0.000000


#configure for mobilenodes
$ns_ node-config -wiredRouting OFF \
		 -routerTrace ON \
                 -macTrace ON \
		 -movementTrace ON 


for {set j 0} {$j < $opt(nn)} {incr j} {
    set node_($j) [ $ns_ node [lindex $temp \
	    [expr $j+1]] ]
    $node_($j) base-station [AddrParams addr2id \
	    [$BS(0) node-addr]]
    $node_($j) base-station [AddrParams addr2id \
	    [$BS(1) node-addr]]
}


$ns_ duplex-link $W(2) $W(0) 500Kb 10ms DropTail
$ns_ duplex-link-op $W(2) $W(0) orient down

$ns_ duplex-link $W(1) $BS(0) 500Kb 10ms DropTail
$ns_ duplex-link-op $W(1) $BS(0) orient down

$ns_ duplex-link $W(2) $BS(1) 500Kb 10ms DropTail
$ns_ duplex-link-op $W(2) $BS(1) orient up

set err [new ErrorModel]
$err drop-target [new Agent/Null] 
$err set rate_ 0.25
[$ns_ link $W(0) $W(2)] errormodule $err

# setup TCP connections
set tcp1 [new Agent/TCP]
$tcp1 set class_ 1
$tcp1 set fid_ 1
set sink1 [new Agent/TCPSink]
$ns_ attach-agent $node_(0) $tcp1
$ns_ attach-agent $W(2) $sink1
$ns_ connect $tcp1 $sink1
set ftp1 [new Application/FTP]
$ftp1 attach-agent $tcp1
$ns_ at $opt(ftp1-start) "$ftp1 start"

set tcp2 [new Agent/TCP]
$tcp2 set class_ 2
$tcp2 set fid_ 2
set sink2 [new Agent/TCPSink]
$ns_ attach-agent $node_(2) $tcp2
$ns_ attach-agent $W(2) $sink2
$ns_ connect $tcp2 $sink2
set ftp2 [new Application/FTP]
$ftp2 attach-agent $tcp2
$ns_ at $opt(ftp2-start) "$ftp2 start"


set tcp3 [new Agent/TCP]
$tcp3 set class_ 3
$tcp3 set fid_ 3
set sink3 [new Agent/TCPSink]
$ns_ attach-agent $node_(1) $tcp3
$ns_ attach-agent $W(2) $sink3
$ns_ connect $tcp3 $sink3
set ftp3 [new Application/FTP]
$ftp3 attach-agent $tcp3
$ns_ at $opt(ftp3-start) "$ftp3 start"

# source connection-pattern and node-movement scripts
if { $opt(cp) == "" } {
	puts "*** NOTE: no connection pattern specified."
        set opt(cp) "none"
} else {
	puts "Loading connection pattern..."
	source $opt(cp)
}
if { $opt(sc) == "" } {
	puts "*** NOTE: no scenario file specified."
        set opt(sc) "none"
} else {
	puts "Loading scenario file..."
	source $opt(sc)
	puts "Load complete..."
}


for {set i 0} {$i < $opt(nn)} {incr i} {
    $ns_ initial_node_pos $node_($i) 30
}     

proc record {} {
        global sink1 sink2 sink3 sink4 sink5 f1 f2 f3 
       
        set ns_ [Simulator instance]
        
        set time 5.0
        
        set bw1 [$sink1 set bytes_]
        set bw2 [$sink2 set bytes_]
        set bw3 [$sink3 set bytes_]
	        

        set now [$ns_ now]
       
        puts $f1 "$now [expr $bw1/$time*8/1000000]"
        puts $f2 "$now [expr $bw2/$time*8/1000000]"
        puts $f3 "$now [expr $bw3/$time*8/1000000]"
        

        $sink1 set bytes_ 0
        $sink2 set bytes_ 0
        $sink3 set bytes_ 0
	        
        $ns_ at [expr $now+$time] "record"
}

$ns_ at 10.1 "record" 
# Tell all nodes when the simulation ends
for {set i } {$i < $opt(nn)} {incr i} {
    $ns_ at $opt(stop).0 "$node_($i) reset";
}
$ns_ at $opt(stop).0 "$BS(0) reset";
$ns_ at $opt(stop).0 "$BS(1) reset";

$ns_ at $opt(stop).0002 "puts \"NS EXITING...\" ; $ns_ halt"
$ns_ at $opt(stop).0001 "stop"
proc stop {} {
    global ns_ tracefd namtrace f1 f2 f3
#    $ns_ flush-trace
    close $tracefd
    close $namtrace
     close $f1
        close $f2
     close $f3
        
        #Call xgraph to display the results
#        exec xgraph noecn1.tr noecn2.tr noecn3.tr noecn4.tr noecn5.tr -geometry 800x400 -x "time" -y "throughput" &
	exec xgraph sn3.tr -geometry 800x400 -x "time" -y "throughput" &

    exec nam snp-bs1.nam &
    exit 0
}


# informative headers for CMUTracefile
puts $tracefd "M 0.0 nn $opt(nn) x $opt(x) y $opt(y) rp \
	$opt(adhocRouting)"
puts $tracefd "M 0.0 sc $opt(sc) cp $opt(cp) seed $opt(seed)"
puts $tracefd "M 0.0 prop $opt(prop) ant $opt(ant)"

puts "Starting Simulation..."
$ns_ run

