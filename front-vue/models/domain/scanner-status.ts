import * as yup from "yup";

const ScannerStatus = ["idle", "scan", "clean"] as const;
type ScannerStatus = (typeof ScannerStatus)[number];

const ScannerStatusResponse = yup.object({
    status: yup.mixed<ScannerStatus>().oneOf(ScannerStatus).required(),
});

type ScannerStatusResponse = yup.InferType<typeof ScannerStatusResponse>;

export { ScannerStatusResponse, ScannerStatus };
