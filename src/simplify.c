/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2016 The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include "grid.h"
#include <math.h>

#define PI 3.14159265

/*
 * Project a point onto a line defined by a point and a slope
 */
void project(double xStart, double yStart, double angle, double *x, double *y) {
    double x0 = cos(xStart);
    double y0 = sin(yStart);
    double x1 = *x - xStart;
    double y1 = *y - yStart;
    double dot = x0 * x1 + y0 * y1;
    *x = x0 * dot + xStart;
    *y = y0 * dot + yStart;
}
/*
 * Calculate the epsilon sector for an anchor point and a new point, given a 
 * specified epsilon
 */
void getEpsilonSector(double x0, double y0, double x1, double y1, double *angle,
                      double *delta, double epsilon) {
    double x = x1 - x0;
    double y = y1 - y0;
    *angle = atan2(y, x);
    *angle = *angle < 0 ? *angle + PI * 2 : *angle;
    double length = sqrt(x * x + y * y);
    *delta = length < epsilon ? PI / 2 : asin(epsilon / length);
}
/*
 * Zhao-Saalfeld line simplification algorithm
 * 
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.494.7321&rep=rep1&type=pdf
 * 
 * Will return 0 if the queried point is inside the given sector bounds while
 * updating the bounds to include the new point. Will return 1 if the point
 * falls outside the sector bounds. In that case xAnchor and yAnchor 
 * (referencing the point that came before) will get projected to the center of 
 * the bounds and a new bound from the projected point to the current point will
 * get initiated
 */
int outsideSector(double x0, double y0, double x1, double y1, double *alpha0, 
                   double *alpha1, double *xAnchor, double *yAnchor, 
                   double epsilon) {
    double angle, delta;
    getEpsilonSector(x0, y0, x1, y1, &angle, &delta, epsilon);
    double new_alpha0 = angle - delta;
    double new_alpha1 = angle + delta;
    if (angle > *alpha0 && angle < *alpha1) {
        *alpha0 = *alpha0 < new_alpha0 ? new_alpha0 : *alpha0;
        *alpha1 = *alpha1 < new_alpha1 ? new_alpha1 : *alpha1;
        return 0;
    }
    project(x0, y0, (*alpha0 + *alpha1) / 2.0, xAnchor, yAnchor);
    getEpsilonSector(*xAnchor, *yAnchor, x1, y1, &angle, &delta, epsilon);
    *alpha0 = angle - delta;
    *alpha1 = angle + delta;
    return 1;
}
