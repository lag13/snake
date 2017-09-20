// Package geom2d has functions and types to deal with 2D geometry.
package geom2d

// Vector has a magnitude and direction. So basically it "points" in a
// direction and it points a certain distance in that direction.
type Vector struct {
	X int
	Y int
}

// AreOrthogonal reports of two vectors are orthogonal to eachother.
func AreOrthogonal(v1 Vector, v2 Vector) bool {
	return v1.X*v2.X+v1.Y*v2.Y == 0
}

// Point is a location in 2D space.
type Point struct {
	X int
	Y int
}

// MovePoint moves a point along a vector.
func MovePoint(p Point, v Vector) Point {
	return Point{
		X: p.X + v.X,
		Y: p.Y + v.Y,
	}
}

// ContainsPoint returns true if the slice contains the specificed
// point.
func ContainsPoint(ps []Point, q Point) bool {
	for _, p := range ps {
		if p == q {
			return true
		}
	}
	return false
}
