package bg.sofia.uni.fmi.mjt.netflix.content;

import bg.sofia.uni.fmi.mjt.netflix.content.enums.Genre;
import bg.sofia.uni.fmi.mjt.netflix.content.enums.PgRating;

public class Movie implements Streamable {

    private final String name;
    private final Genre genre;
    private final PgRating rating;
    private final int duration;

    public Movie(final String name, final Genre genre, final PgRating rating, final int duration) {
        this.name = name;
        this.genre = genre;
        this.rating = rating;
        this.duration = duration;
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public int getDuration() {
        return duration;
    }

    @Override
    public PgRating getRating() {
        return rating;
    }
}
