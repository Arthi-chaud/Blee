import type { Chapter } from "~/models/domain/chapter";

export const useCurrentChapter = ({
    chapters,
    progress,
    onChapterChange,
}: {
    chapters: Ref<Chapter[]> | Ref<undefined>;
    onChapterChange: () => void;
    progress: Ref<number>;
}) => {
    const currentChapter = ref<Chapter | null>();
    const updateCurrentChapter = () => {
        if (!progress.value) {
            return;
        }
        const oldChapterId = currentChapter.value?.id;
        const newChapter = chapters.value?.find(
            (c) =>
                c.start_time <= progress.value && progress.value < c.end_time,
        );
        currentChapter.value = newChapter;
        if (newChapter?.id !== oldChapterId || !oldChapterId) {
            onChapterChange();
        }
    };
    const cId = setInterval(() => {
        if (!progress.value) {
            return;
        }
        if (!currentChapter.value) {
            updateCurrentChapter();
        } else if (
            progress.value < currentChapter.value.start_time ||
            progress.value > currentChapter.value.end_time
        ) {
            updateCurrentChapter();
        }
    }, 1000);
    onBeforeUnmount(() => {
        clearInterval(cId);
    });
    return {
        currentChapter,
    };
};
