package com.valentinvstoqnov;

public class SocialDistanceMaximizer {

    public static int maxDistance(int[] seats) {
        int maxStart = 1;
        int maxEnd = 0;
        int currStart = 0;
        int currEnd = 0;

        for (int i = 0; i < seats.length; ++i) {
            if (seats[i] == 0) {
                ++currEnd;
                if (i == seats.length - 1 && currEnd - currStart > maxEnd - maxStart) {
                    maxEnd = currEnd;
                    maxStart = currStart;
                }
            } else if (seats[i] == 1) {
                if (currEnd - currStart > maxEnd - maxStart) {
                    maxEnd = currEnd;
                    maxStart = currStart;
                }
                currStart = i + 1;
                currEnd = i + 1;
            }
        }

        int maxDist = maxEnd - maxStart;
        if (maxStart > 0 && maxEnd < seats.length - 1) {
            if (maxDist % 2 == 0) {
                maxDist = maxDist / 2;
            } else {
                maxDist = maxDist / 2 + 1;
            }
        }

        return maxDist;
    }
}
