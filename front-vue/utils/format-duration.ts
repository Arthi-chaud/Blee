const format = (n: number) => n.toString().padStart(2, "0");
export const formatDuration = (timeInSeconds: number) => {
    const hours = Math.floor(timeInSeconds / 3600);
    const minutes = Math.floor(timeInSeconds / 60) % 60;
    const seconds = timeInSeconds % 60;

    if (hours) {
        return `${hours}:${format(minutes)}:${format(seconds)}`;
    }
    return `${format(minutes)}:${format(seconds)}`;
};
