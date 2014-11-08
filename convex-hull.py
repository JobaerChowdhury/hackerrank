#-- Input: a list P of points in the plane.

#-- Sort the points of P by x-coordinate (in case of a tie, sort by y-coordinate).

#-- Initialize U and L as empty lists.
#-- The lists will hold the vertices of upper and lower hulls respectively.

#-- for i = 1, 2, ..., n:
#--     while L contains at least two points and the sequence of last two points
#--            of L and the point P[i] does not make a counter-clockwise turn:
#--        remove the last point from L
#--    append P[i] to L

#-- for i = n, n-1, ..., 1:
#--    while U contains at least two points and the sequence of last two points
#--            of U and the point P[i] does not make a counter-clockwise turn:
#--        remove the last point from U
#--    append P[i] to U

#-- Remove the last point of each list (it's the same as the first point of the other list).
#-- Concatenate L and U to obtain the convex hull of P.
#-- Points in the result will be listed in counter-clockwise order.


def convex_hull(points):
    """Computes the convex hull of a set of 2D points.
 
    Input: an iterable sequence of (x, y) pairs representing the points.
    Output: a list of vertices of the convex hull in counter-clockwise order,
      starting from the vertex with the lexicographically smallest coordinates.
    Implements Andrew's monotone chain algorithm. O(n log n) complexity.
    """
 
    # Sort the points lexicographically (tuples are compared lexicographically).
    # Remove duplicates to detect the case we have just one unique point.
    points = sorted(set(points))
 
    # Boring case: no points or a single point, possibly repeated multiple times.
    if len(points) <= 1:
        return points
 
    # 2D cross product of OA and OB vectors, i.e. z-component of their 3D cross product.
    # Returns a positive value, if OAB makes a counter-clockwise turn,
    # negative for clockwise turn, and zero if the points are collinear.
    def cross(o, a, b):
        return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])
 
    # Build lower hull 
    lower = []
    for p in points:
        while len(lower) >= 2 and cross(lower[-2], lower[-1], p) <= 0:
            lower.pop()
        lower.append(p)
    print lower
 
    # Build upper hull
    upper = []
    for p in reversed(points):
        while len(upper) >= 2 and cross(upper[-2], upper[-1], p) <= 0:
            upper.pop()
        upper.append(p)
    print upper
 
    # Concatenation of the lower and upper hulls gives the convex hull.
    # Last point of each list is omitted because it is repeated at the beginning of the other list. 
    return lower[:-1] + upper[:-1]
 
 
# Example: convex hull of a 10-by-10 grid.
ps1 = [(5,10),(10,5),(15,5), (20,10),(15,15),(10,15)]
ps2 = [(1,1), (2,5), (3,3), (5,3), (3,2),(2,2)]
print convex_hull(ps2)
# assert convex_hull([(i/10, i%10) for i in range(100)]) == [(0, 0), (9, 0), (9, 9), (0, 9)]