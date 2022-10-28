difference() {
    cube([30, 30, 5]);
    translate([ 6, 6,-2.5]) cylinder(10, 5.7 / 2, 5.7 / 2, $fn=32);
    translate([ 6,14,-2.5]) cylinder(10, 5.8 / 2, 5.8 / 2, $fn=32);
    translate([ 6,22,-2.5]) cylinder(10, 5.9 / 2, 5.9 / 2, $fn=32);
    translate([14, 6,-2.5]) cylinder(10, 6.0 / 2, 6.0 / 2, $fn=32);
    translate([14,14,-2.5]) cylinder(10, 6.1 / 2, 6.1 / 2, $fn=32);
    translate([22,14,-2.5]) cylinder(10, 6.2 / 2, 6.2 / 2, $fn=32);
    translate([22,22,-2.5]) cylinder(10, 6.3 / 2, 6.3 / 2, $fn=32);
}