package bg.sofia.uni.fmi.mjt.netflix.content;

import bg.sofia.uni.fmi.mjt.netflix.content.enums.Genre;
import bg.sofia.uni.fmi.mjt.netflix.content.enums.PgRating;

public class Series implements Streamable {

    private final String name;
    private final Genre genre;
    private final PgRating rating;
    private final Episode[] episodes;
    private int duration = -1;

    public Series(final String name, final Genre genre, final PgRating rating, final Episode[] episodes) {
        this.name = name;
        this.genre = genre;
        this.rating = rating;
        this.episodes = episodes;
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public int getDuration() {
        if (duration < 0) {
            duration = 0;
            for (Episode episode : episodes) {
                duration += episode.getDuration();
            }
        }
        return duration;
    }

    @Override
    public PgRating getRating() {
        return rating;
    }
}
