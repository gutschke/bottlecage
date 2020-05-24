// Recreation of the Prusa ASA bottle cage in OpenSCAD
// Copyright 2020 by M. Gutschke
// Released into the public domain under the Creative Commons CC0
// license included by reference.


// WARNING!
//
// THIS IS A MODIFIED VERSION OF THE REGULAR BOTTLE CAGE FILE. IT DEMONSTRATES
// SOME AD HOC CHANGES TO MAKE THE CAGE FIT INTO VERY CONSTRAINT NON-STANDARD
// SPACES. USE FOR REFERENCE, BUT I STRONGLY SUGGEST WORKING FROM THE UNMODIFIED
// REFERENCE IMPLEMENTATION INSTEAD!

$fa = 1;
$fs = 0.4;

hasslot = false;        // Should there be multiple adjustable mount points?

// The following adjustable parameters are in inches
diameterin = 2.6;       // Diameter of bottle (common: 2.875 and 2.6)
bracketheightin = 5;    // Height of bracket
bracketwidthin = 1;     // Width of bracket attaching to bike frame
brackettopin = 0.1;     // Radius of reinforcements at top of bracket   
brackettaperin = 0.075; // Bracket tapers in by a few millimeters
screwdistancein = 2.5;  // Distance between primary mounting holes
heightin = 6;           // Overall height of entire bottle cage
framediameterin = 3/2;  // Diameter of bicycle frame

// The following adjustable parameters are in millimeters
wallthick = 6;          // Wall at thickest part (adds strength)
wallthin = 3.4;         // Wall at thinnest part (adds flexibility)
roundover = 1.4;        // Radius for rounding over some of the edges
screwdiameter = 5;      // M5 mounting screw


// This module generates a double-helix shape that extends all the way down to
// the XY plane. It can be used to cut away shapes from cylinders to form
// different helix strips. It is used by the doublehelix() module. The module
// returns a polyhedron that is comprised of two 180° section each twisting in
// opposite directions. The total height is "h" and the radius is "r".
// Both the bottom and the top of the belix can optionally be rounded-over. To do
// so, the caller must provide a radius for the amount of round-over.
module helixbottom(r, h, r0 = 0, r1) {
  r1_ = r1 == undef ? r0 : r1;

  // Compute the number of steps based on the standard OpenSCAD parameters.
  steps = ceil($fn > 0 ? $fn/2 : min(PI*r/$fs, 180/$fa));

  // In order to avoid rounding errors, we iterate in units of steps. But in
  // order to do math, we need to convert steps to degrees around the helix.
  // The output values of the conversion function cover the range [0:180].
  function deg(s) = round(180*s/steps);

  // The height should not vary linearly. Instead, we want to round-over the start
  // and the end of the curve. This requires a little bit of math to find the
  // elements of the piecewise step function.
  // The total height of the step function is made up of two circle segments at
  // the beginning and at the end of the height-curve, respectively; these segments
  // are connected by a linear section of constant slope. In order to ensure a
  // seamless transition, we need to determine the inner tangent connecting the
  // two circles for both the start and finish point.
  // The first value we compute is the total length along the perimeter (as
  // projected onto the XY plane). This becomes the X axis for our calculation of
  // the height curve.
  p = PI*r;

  // Center point for the circle segment at the start of the height curve.
  p0x = 0; p0y = r0;

  // Center point for the circle segment at the finish of the height curve.
  p1x = p; p1y = h - r1_;

  // Point in the middle between these two center points; this is needed to
  // calculate the inner tangent touching both circles. N.b. the tangent typically
  // would not run through this point. It is merely an auxiliary coordinate to
  // do the necessary calculations.
  pcx = p/2; pcy = (h - r1_ + r0)/2;

  // We imagine two circles that will help us compute the tangent. The first circle
  // is centered around [p0x,p0y], but has a radius of "r0+r1". The second
  // circle is centered around [pcx,pcy], and touches both [p0x,p0y] and [p1x,p1y].
  // The square of the radius of this inscribing circle is "d2". That is also by
  // definition the distance between the inscribing circle and the two circle
  // segments at each end of our height curve.
  d2 = pcx*pcx + pow(pcy - p0y, 2);

  // In order to compute the tangent, we need to know the intersection points
  // of our two auxiliary circles. There are two of these points, be we only
  // need one of them.
  // The computation has a common factor "d" that we calculate up front.
  d = (r0 + r1_)*sqrt(4*d2 - pow(r0 + r1_, 2))/4;

  // Now we are finally ready to compute the intersection point. Again, this is
  // only another auxiliary point that will help compute the tangent.
  pix = (p0x + pcx)/2 + (pcx - p0x)*(pow(r0 + r1_, 2) - d2)/(2*d2) -
        d*(r0 + r1_ - h)/d2;
  piy = (p0y + pcy)/2 + (pcy - p0y)*(pow(r0 + r1_, 2) - d2)/(2*d2) - d*p/d2;

  // The tangent touches the starting circle segment where it intersects with the
  // line between that circle's center and the point [pix,piy] that we just
  // computed. We can find it by computing a vector of length "r0" from the
  // circle's center in the direction of our helper line. In the case of "r0"
  // being zero, we perform an alternate calculation that assumes that the
  // tangent passes through the origin.
  l = r0 > 0 ? r0 / sqrt(pix*pix + pow(piy - r0, 2)) : r1_/sqrt(p1x*p1x + p1y*p1y);
  xl0 = r0 > 0 ? l*pix : 0;
  yl0 = r0 > 0 ? r0 + l*(piy - r0) : 0;

  // Now that we know where the tangent touches the starting circle segment,
  // we also know its slope. This value is perpendicular to our helper line
  // through the circle segment's center and thus easy to compute through
  // geometric construction. In the case of a zero "r0" radius, we again fall
  // back on an alternative construction that relies on the tangent passing
  // through the origin.
  slope = r0 > 0 ? pix/(r0 - piy) : (p1y + l*p1x)/(p1x - l*p1y);

  // Knowing the slope, we can use a similar argument to derive the point where the
  // inner tangent touches the finishing circle segment.
  k = r1_/sqrt(slope*slope + 1);
  xl1 = p - slope*k;
  yl1 = h - r1_ + k;

  // Now we have enough information to assemble our piecewise function.
  function f(x) = x < xl0 ? r0 - sqrt(r0*r0 - x*x)
                : x > xl1 ? h - r1_ + sqrt(r1_*r1_ - pow(p - x, 2))
                : slope*(x - xl0) + yl0;

  // We have to build the entire shape as a single polyhedron. Trying to compose
  // from individual parts makes it more likely to introduce edge cases that don't
  // render correctly.
  polyhedron(
    // We divide the helix shape into arc segments around the center of the
    // cylinder.
    // For each segment, we compute 5 different vertices that will later be used to
    // make up a variety of surfaces.
    points = [ for (s = [0:steps]) each [
      // The following are points on the outside of the helix, gradually
      // increasing in height, as the angle covers the range [0..180]. We also
      // compute the mirror image for the reverse-spin helix.
      [ -r*cos(deg(s)),  r*sin(deg(s)), f(p*s/steps) ],
      [ -r*cos(deg(s)), -r*sin(deg(s)), f(p*s/steps) ],

      // Vertex in the center of the helix, at the same height as the previous
      // points.
      [ 0, 0, h*s/steps ],

      // Projection of the two rotating vertices onto the XY plane.
      [ -r*cos(deg(s)),  r*sin(deg(s)), 0 ],
      [ -r*cos(deg(s)), -r*sin(deg(s)), 0 ]]],

    // Make triangles or faces from the vertices. Pay special attention to the
    // correct orientation of the normal vector. Avoid interior surfaces and
    // duplicates.
    // Iterate in steps of five, as the vertices are arranged in groups of five.
    faces = [ for (i = [0:5:5*(steps-1)]) each [
      // The helical surface cut away from the cylinder is made up of zig-zagging
      // triangles that connect the perimeter of the cylinder to the axis running
      // through its center. The very first (i.e. bottom-most) triangle is special
      // though. It happens to be shared with the helical surface of the
      // opposite-spin helix. In fact, it not only overlaps with that mirrored
      // triangle, these triangles both are fully located inside of the
      // polyhedron. Make sure to skip this triangle to avoid rendering problems.
      each [if (i > 0) [i, i+7, i+2]], [ i, i+5, i+7],

      // Add faces around the perimeter of the cylinder, dropping down from the
      // helical surface to the XY plane. The very first face touches the XY
      // plane with one of its edges and the perimeter face is therefore
      // degenerate. The normal quad is actually just a triangle as two of the
      // vertices coincide. Removing that extra vertex avoids rendering errors.
      [ each [if (i > 0) i], i+3, i+8, i+5 ],

      // The bottom view of our polyhedron is a circle. Construct it from triangles
      // instead of having OpenSCAD build it from a "circle()" primitve. This way,
      // we precisely control vertex locations and avoid unexpected gaps.
      [ 0, i+8, i+3],

      // Repeat all of the above for the mirrored opposite-spin helix. Make sure
      // to use shared vertices where applicable.
      [ 0, i+4, i+9],
      [ each [if (i > 0) i+1], i+6, i+9, i+4 ],
      each [if (i > 0) [i+1, i+2, i+7]], [i+1, i+7, i+6]]]);
}

// This module uses the helixbottom primitive to make a 180° section of a double
// helix. Up to four seperate round-over radii can optionally be specified. If
// some or all are left out, the module fills in defaults (or copies the other
// parameters as applicable).
module doublehelix(r, h, t0, t1, w0, w1, r0 = 0, r1, r2, r3) {
  difference() {
    // The ribbon width can have one or two values
    h1 = h - (w1 == undef ? w0 : w1);

    // And so can the thickness
    t1_ = t1 == undef ? t0 : t1;

    // Make a hollowed-out cylinder
    cylinder(r = r, h = h);
    translate([(t0 - t1_)/2, 0, -0.05])
      cylinder(r = r - (t0 + t1_)/2, h = h + 0.1);

    // Then cut away the bottom of the helix
    union() {
      helixbottom(r = r + 1, h = h1, r0 = r0, r1 = r1);
      translate([0, 0, -0.05]) cylinder(r = r + 1, h = 0.06);
    }

    // Now do the same for the top edge of the helix
    translate([0, 0, w0])
      difference() {
        cylinder(r = r + 1, h = h - w0 + 0.1);
        helixbottom(r = r + 2, h = h - w0, r0 = r2 == undef ? r0 : r2, r1 = r3);
      }
  }
}

// Diameter of bottle including some extra slack
diameter = 25.4 * diameterin + 2.5;

// Total bracketheight plus some extra reinforcement
bracketheight = 25.4 * bracketheightin + 2*wallthick;

// Width of bracket attaching to bike frame
bracketwidth = 25.4 * bracketwidthin;

// Radius of reinforcements at top of bracket
brackettop = 25.4 * brackettopin;

// Bracket tapers in by a few millimeters
brackettaper = 25.4 * brackettaperin;

// Distance between screws
screwdistance = 25.4 * screwdistancein;

// Overall height of entire bottle cage
height = 25.4 * heightin;

// Diameter of bicycle frame
framediameter = 25.4 * framediameterin;

// Dimensions of top loop
toploopheight = height/sqrt(2);
toploopoffset = bracketheight - toploopheight + 1.25;

// The basic shape of the bottle cage can be assembled from its individual
// components (two double-helix loops, a back bracket and a bottom brace). But
// at least one of the transitions, where the top loop meets the bottom brace is
// hard to do without sharp angles. We add the missing transition as a
// post-processing step and thus have to wrap the construction of the basic bottle
// cage into a module.
module bottlecage_() {
  difference() {
    // Outer diameter of double helix
    outerdiameter = diameter + wallthick + wallthin;

    // The thickness of the bracket aligns with the loop as it exits the bracket
    bracketthickness = outerdiameter/2-(diameter/2)*
                       sqrt(1-pow((bracketwidth/2-brackettaper/4)/(diameter/2), 2));

    // The bottom brace has a similar issue where it transitions from
    // horizontal to vertical
    bracethickness = wallthick+diameter/2*(1-sqrt(1-pow(wallthick/diameter, 2)));

    // Combine all the components, so that we can use difference() to make openings
    union() {
      // Bottom loop
      difference() {
        // Shift down to make nicer connection with bracket
        translate([outerdiameter/2-0.4, 0, -3])
          doublehelix(r = outerdiameter/2, h = height+5,
                      // Part at bracket is strong; outer part is more flexible
                      t0 = wallthick, t1 = wallthin,
                      // Loop attached to bracket is wider than free section
                      w0 = 45, w1 = 11,
                      // Round over all corners to look more organic
                      r0 = 20, r1 = 5, r2 = 20, r3 = 20);
        
        // We moved the loop into a visually pleasing location, where is
        // attaches  to the bracket. This results in some sections spilling
        // over, where they shouldn't be. Cut off everything below the XY plane.
        translate([-10, -100, -100])
          cube([100, 200, 100]);
      }
 
      // Top loop
      difference() {
        // This is pretty much the same as the bottom loop just turned 180° around
        // the Z axis. Also, unlike the bottom loop which thins out at the open top
        // the top loop maintains a constant thickness of "wallthick" throughout.
        // This will require adjustments to various dimensions throughout the rest
        // of the model (e.g. for the bottom brace).
        translate([outerdiameter/2-0.4+(wallthick-wallthin)/2, 0, toploopoffset])
        rotate([0, 0, 180])
          doublehelix(r=outerdiameter/2 + (wallthick - wallthin)/2,
                      h = toploopheight,
                      t0 = wallthick, t1 = wallthick,
                      w0 = 9, w1 = 24,
                      r0 = 15, r1 = 25, r2 = 10, r3 = 25);
 
        // Clean up any excess material from the loop that extends past the top
        // of the bracket
        translate([-10, -100, bracketheight])
          cube([100, 200, 100]);
      }
      
      // Bracket attaching the bottle cage to the bike frame
      difference() {
        union() {
          // The body of the bracket is tapered and has rounded over edges
          linear_extrude(bracketheight)
            hull() {
              translate([roundover, roundover-bracketwidth/2]) circle(roundover);
              translate([bracketthickness-roundover,
                         roundover-bracketwidth/2+brackettaper]) circle(roundover);
              translate([bracketthickness-roundover,
                        -roundover+bracketwidth/2-brackettaper]) circle(roundover);
              translate([roundover,-roundover+bracketwidth/2]) circle(roundover);
          }

          // Put some extra reinforcements around the top of the bracket
          for (i = [1, -1])
            translate([outerdiameter/2-0.4, 0, 0])
              rotate([0, 0,
                     i*atan((bracketwidth/2-roundover/4)/(outerdiameter/2-0.4))])
              translate([-(outerdiameter/2-0.4)-(wallthick - wallthin - 0.9)/4,
                         0, bracketheight - 0.9*brackettop])
              rotate([0, 90, 0]) {
                cylinder(r = brackettop, h = 2*wallthick);
                translate([0, -brackettop, 0])
                  cube([brackettop, 2*brackettop, 2*wallthick]);
            }
        }

        // We raised the reinforcement a little bit, to clean up some joints.
        // Now we need to remove excess material
        translate([-10, -100, bracketheight])
          cube([100, 200, 100]);
      }
   
      // Brace at the bottom of the cage, preventing the bottle from falling out
      linear_extrude(height = wallthick)
        difference() {
          // Basic tapered shape of bottom brace
          polygon([[bracketthickness/4,  bracketwidth/2],
                   [bracketthickness,  bracketwidth/2],
                   [outerdiameter - 0.4 - bracethickness - wallthin - 4,  wallthick/2],
                   [outerdiameter - 0.4 - wallthin - 4,  wallthick/2],
                   [outerdiameter - 0.4 - wallthin - 4, -wallthick/2],
                   [outerdiameter - 0.4 - bracethickness - wallthin - 4, -wallthick/2],
                   [bracketthickness, -bracketwidth/2],
                   [bracketthickness/4, -bracketwidth/2]]);

          // Sweep the taper in a more rounded shape. We do this by computing
          // large-diameter circles that touch the two end-points of the tapered
          // section of the brace.
          r = 3 * outerdiameter;
          d = sqrt(pow(bracketthickness - (outerdiameter - 0.4 - bracethickness - wallthin), 2) +
                       pow(bracketwidth/2 - wallthick/2, 2));
          h = sqrt(pow(r, 2) - pow(d/2, 2));
          x = (outerdiameter - 0.4 - bracethickness - wallthin - bracketthickness)/2 -
              h/d*(wallthick/2 - bracketwidth/2) + bracketthickness;
          y = (wallthick - bracketwidth)/4 + bracketwidth/2 +
              h/d*(outerdiameter - 0.4 - bracethickness - wallthin - bracketthickness);
          translate([x, y]) circle(r = r);
          translate([x,-y]) circle(r = r);
        }
      
      // Where the brace transitions from vertical to horizontal, we are dealing
      // with several different geometric shapes. This will be done in two parts.
      // The additive part is here. And a little further down is the section
      // where we remove all the excess material.
      intersection() {
        union() {
          // Rounded transition from vertical to horizontal. This will give us the
          // outside round edge at the bottom of the brace.
          translate([outerdiameter - 0.4 - wallthin - bracethickness + wallthick - 6, 0, wallthick + 6])
            rotate([90, 0, 0])
            cylinder(r = bracethickness*2, h = wallthick, center = true);
          
          // Vertical part of the brace  
          translate([(outerdiameter + diameter)/2 - 0.4 + 1.5*wallthick - wallthin/2 - bracethickness, -wallthick/2, wallthick+4])
            cube([bracethickness, wallthick, toploopoffset-4]);
          }
       // Intersect the vertical section of the brace with a cylinder encompassing
       // the entire bottle cage. This gives the previously cube-shaped section a
       // nice round outer edge.
       translate([outerdiameter/2 - 0.4 + (wallthick - wallthin)/2, 0, 0])
          cylinder(r = diameter/2 + wallthick, height);
      }
    }

    // Make space for the bicycle frame, so that the bracket can attach without
    // wobbling from side to side
    translate([-sqrt(pow(framediameter/2, 2) -
                     pow((bracketwidth)/2 - brackettaper, 2)) - 0.5, 0, 0])
      cylinder(r = framediameter/2, h = bracketheight);

    // Drill mounting holes
    for (z = [ wallthick + (bracketheight + screwdistance)/2 - 2,
               each [if (hasslot) bracketheight - bracketwidth/2]])
      translate([0, 0, z]) {
        rotate([0, 90, 0])
          cylinder(r = screwdiameter/2 + 0.25, h = bracketthickness);
        translate([wallthick - screwdiameter/4, 0, 0])
          rotate([0, 90, 0])
            cylinder(r = screwdiameter + 0.25, h = bracketthickness);
    }
    // And an elongated slot for non-standard mounting holes
    hull() {
      for (z = [wallthick + (bracketheight - screwdistance)/2 - 2,
                each [if (hasslot) bracketwidth/2 + wallthick]])
      translate([0, 0, z])
        rotate([0, 90, 0])
          cylinder(r = screwdiameter/2 + 0.25, h = bracketthickness);
    }
    hull() {
      for (z = [wallthick + (bracketheight - screwdistance)/2 - 2,
                each [if (hasslot) bracketwidth/2 + wallthick]])
        translate([wallthick - screwdiameter/4, 0, z])
          rotate([0, 90, 0])
            cylinder(r = screwdiameter + 0.25, h = bracketthickness);
    }

    // Carve out space on the inside of the cage for the bottle
    union() {
      difference() {
        translate([outerdiameter/2 - 0.4 + (wallthick - wallthin)/2, 0, wallthick])
          cylinder(r = diameter/2, h = bracketheight - wallthick);

        // The transition of the bottom brace from horizontol to vertical is a
        // complex shape. We do that in a separate step. Notch out a section in
        // the cylinder, so that we don't remove more material than we should.
        translate([outerdiameter - 0.4 - wallthin - wallthick - 5,
                   -5*wallthick, wallthick])
          cube([wallthick + 5, 10*wallthick, wallthick]);
      }

      // Gradually translate the shape that we cut out by transitioning from a
      // vertical cylinder (the bottle) to a horizontal cylinder (the bent
      // section of the bottom brace). A hull can make this gradually change.
      hull() {
        translate([outerdiameter - 0.4 - wallthick - wallthin - 5, -wallthick/2,
                   2*wallthick])
          rotate([-90, 0, 0])
          resize([2*wallthick+10, 2*wallthick, wallthick])
          cylinder(r = wallthick, h = wallthick);
        translate([outerdiameter/2 - 0.4 + (wallthick - wallthin)/2, 0,
                   2*wallthick])
            cylinder(r = diameter/2, h = bracketheight - wallthick);
      }
    }

    // Cleanly finish the bottom of the bracket
    rotate([0, 45, 0])
      cube([100, 100, bracketthickness/2], center=true);
  }
}

module bottlecage() {
  // The bottle cage is almost perfect, if it wasn't for the missing transition
  // from the bottom of the top loop to the vertical section of the bottom
  // brace.
  // These shapes are too complex for us to do a perfect fit. But by cutting
  // things apart and reassembling them, we can come really close.
  bottlecage_();

  difference() {
    // Cut out a section of the bottlecage that needs a sweeping transition.
    // Then put a hull around it. Afterwards, cut round shapes from the
    // sides of the hull and overlay the entire structure ontop of
    // the original bottle cage.
    hull() {
      intersection() {
        bottlecage_();

        // This part of the code uses approximated values, as the complex shape
        // of the loop makes it hard to perfectly compute the coordinates. Unless
        // there are large adjustments to the overall dimensions, the
        // inaccuracies should be really minute though.
        translate([diameter - 0.4, 1-3*wallthick, 3*wallthick])
          cube([2*wallthick, 6*wallthick - 2, toploopoffset + 9 - 3*wallthick]);
      }
    }
    
    // Now that we have a section of the bottle cage with a skin stretched over it
    // by the hull() operator, we can remove the space that will be occupied by the
    // bottle. It was erroneously added by hull().
    translate([diameter/2 - 0.4 + wallthick, 0, wallthick])
      cylinder(r = diameter/2, h = bracketheight - wallthick);
 
    // Sweep the transition by cutting out two large cylinder shapes touching the
    // hull in just the corners. Again, there are unfortunately a few empirically
    // determined values here. There is only so much we can compute.
    r = 5 * wallthick;
    d = sqrt(pow(1.8*wallthick, 2) + pow(toploopoffset + 9 - 3*wallthick, 2));
    h = sqrt(pow(r, 2) - pow(d/2, 2));
    x = 0.9*wallthick + h/d*(toploopoffset + 9 - 3*wallthick) + wallthick/2;
    y = (toploopoffset + 9 - 3*wallthick)/2 - h/d*1.8*wallthick + 3*wallthick;
    for (i = [-1, 1])
      translate([diameter/2 - 0.4,  i*x, y])
      rotate([0, 90, 0])
        cylinder(r = r, h = diameter);
  }
}

bottlecage();