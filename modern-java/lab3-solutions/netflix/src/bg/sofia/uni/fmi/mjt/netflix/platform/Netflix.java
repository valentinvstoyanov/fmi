package bg.sofia.uni.fmi.mjt.netflix.platform;

import bg.sofia.uni.fmi.mjt.netflix.account.Account;
import bg.sofia.uni.fmi.mjt.netflix.content.Streamable;
import bg.sofia.uni.fmi.mjt.netflix.content.enums.PgRating;
import bg.sofia.uni.fmi.mjt.netflix.exceptions.ContentNotFoundException;
import bg.sofia.uni.fmi.mjt.netflix.exceptions.ContentUnavailableException;
import bg.sofia.uni.fmi.mjt.netflix.exceptions.UserNotFoundException;

public class Netflix implements StreamingService {

    private final Account[] accounts;
    private final Streamable[] streamableContent;
    private final int[] contentViews;
    private int mostViewedContentIdx = -1;
    private int totalWatchDuration = 0;

    public Netflix(Account[] accounts, Streamable[] streamableContent) {
        this.accounts = accounts;
        this.streamableContent = streamableContent;
        this.contentViews = new int[streamableContent.length];
    }

    private int findAccountIndex(Account user) {
        for (int i = 0; i < accounts.length; ++i) {
            if (user.getUsername().equals(accounts[i].getUsername())) {
                return i;
            }
        }
        return -1;
    }

    private int findContentIndexByName(String videoContentName) {
        for (int i = 0; i < streamableContent.length; ++i) {
            if (videoContentName.equals(streamableContent[i].getTitle())) {
                return i;
            }
        }
        return -1;
    }

    private boolean passesAgeRestriction(Account user, PgRating rating) {
        switch (rating) {
            case G: return true;
            case PG13: return user.getAge() > 13;
            case NC17: return user.getAge() > 18;
            default: return false;
        }
    }

    private void updateMostViewed(int contentIdx) {
        if (mostViewedContentIdx < 0) {
            mostViewedContentIdx = contentIdx;
            return;
        }

        if (contentViews[contentIdx] > contentViews[mostViewedContentIdx]) {
            mostViewedContentIdx = contentIdx;
        }
    }

    private void incrementViewsFor(int contentIdx) {
        contentViews[contentIdx] += 1;
        updateMostViewed(contentIdx);
    }

    @Override
    public void watch(Account user, String videoContentName) throws ContentUnavailableException {
        int accountIdx = findAccountIndex(user);
        if (accountIdx < 0) {
            throw new UserNotFoundException();
        }

        int contentIdx = findContentIndexByName(videoContentName);
        if (contentIdx < 0) {
            throw new ContentNotFoundException();
        }

        Streamable content = streamableContent[contentIdx];
        if (!passesAgeRestriction(user, content.getRating())) {
            throw new ContentUnavailableException();
        }

        totalWatchDuration += content.getDuration();
        incrementViewsFor(contentIdx);
    }

    @Override
    public Streamable findByName(String videoContentName) {
        int contentIdx = findContentIndexByName(videoContentName);
        return contentIdx >= 0 ? streamableContent[contentIdx] : null;
    }

    @Override
    public Streamable mostViewed() {
        return mostViewedContentIdx >= 0 ? streamableContent[mostViewedContentIdx] : null;
    }

    @Override
    public int totalWatchedTimeByUsers() {
        return totalWatchDuration;
    }
}
