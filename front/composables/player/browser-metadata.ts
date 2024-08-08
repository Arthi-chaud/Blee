import { API } from "~/api/api";
import type { Chapter } from "~/models/domain/chapter";
import type { Extra } from "~/models/domain/extra";
import type { Movie } from "~/models/domain/movie";
import type { Package } from "~/models/domain/package";
type R<T> = Ref<T> | Ref<undefined>;

export const useBrowserMetadata = (props: {
    extra: R<Extra>;
    movie: R<Movie>;
    currentChapter: R<Chapter | null>;
    parentPackage: R<Package>;
}) => {
    const { extra, movie, parentPackage, currentChapter } = props;
    const setMediaMetadata = () => {
        if (typeof navigator.mediaSession === "undefined") return;
        navigator.mediaSession.metadata = new MediaMetadata({
            title:
                extra.value?.name ??
                currentChapter.value?.name ??
                movie.value?.name,
            album: extra.value ? parentPackage.value?.name : movie.value?.name,
            artist:
                (movie.value ?? extra.value ?? parentPackage.value)
                    ?.artist_name ?? undefined,
            artwork: parentPackage.value?.poster
                ? [
                      {
                          src: API.buildImageUrl(parentPackage.value.poster.id),
                      },
                  ]
                : undefined,
        });
    };
    return { setMediaMetadata };
};
