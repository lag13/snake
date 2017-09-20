package geom2d_test

import (
	"testing"

	"github.com/lag13/snake/go/geom2d"
)

// TestAreOrthogonal tests that two vectors are correctly reported as
// being orthogonal.
func TestAreOrthogonal(t *testing.T) {
	tests := []struct {
		testScenario string
		v1           geom2d.Vector
		v2           geom2d.Vector
		want         bool
	}{
		{
			testScenario: "vectors are not orthogonal",
			v1:           geom2d.Vector{X: 1, Y: 1},
			v2:           geom2d.Vector{X: 0, Y: 1},
			want:         false,
		},
		{
			testScenario: "vectors are orthogonal",
			v1:           geom2d.Vector{X: 1, Y: 0},
			v2:           geom2d.Vector{X: 0, Y: 1},
			want:         true,
		},
	}
	for i, test := range tests {
		errorMsg := func(str string, args ...interface{}) {
			t.Helper()
			t.Errorf("Running test %d, where %s:\n"+str, append([]interface{}{i, test.testScenario}, args...)...)
		}
		got := geom2d.AreOrthogonal(test.v1, test.v2)
		if got != test.want {
			errorMsg("wanted the orthogonality of vectors %v and %v to be %v", test.v1, test.v2, test.want)
		}
	}
}

// TestMovePoint tests that adding a point and a vector works as
// expected.
func TestMovePoint(t *testing.T) {
	p := geom2d.Point{X: 1, Y: -10}
	v := geom2d.Vector{X: 10, Y: 9}
	if got, want := geom2d.MovePoint(p, v), (geom2d.Point{X: 11, Y: -1}); got != want {
		t.Errorf("got point %+v, wanted %+v", got, want)
	}
}

// TestContainsPoint tests whether a point exists in a slice of
// points.
func TestContainsPoint(t *testing.T) {
	tests := []struct {
		testScenario string
		points       []geom2d.Point
		point        geom2d.Point
		want         bool
	}{
		{
			testScenario: "the point is in the slice",
			points:       []geom2d.Point{{X: 1, Y: 1}, {X: 0, Y: 1}},
			point:        geom2d.Point{X: 0, Y: 1},
			want:         true,
		},
		{
			testScenario: "the point is not in the slice",
			points:       []geom2d.Point{{X: 1, Y: 0}},
			point:        geom2d.Point{X: 0, Y: 1},
			want:         false,
		},
	}
	for i, test := range tests {
		errorMsg := func(str string, args ...interface{}) {
			t.Helper()
			t.Errorf("Running test %d, where %s:\n"+str, append([]interface{}{i, test.testScenario}, args...)...)
		}
		got := geom2d.ContainsPoint(test.points, test.point)
		if got != test.want {
			errorMsg("wanted slice %+v to contain point %+v to be %v", test.points, test.point, test.want)
		}
	}
}
