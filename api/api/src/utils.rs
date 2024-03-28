use rocket::{fs::TempFile, tokio::fs};
use tempfile::NamedTempFile;

use crate::error_handling::ApiError;

pub async fn temp_file_to_bytes_vec(mut data: TempFile<'_>) -> Result<Vec<u8>, ApiError> {
	let tmp_file = NamedTempFile::new().map_err(|_| ApiError::ImageProcessingError)?;

	let _ = data.persist_to(tmp_file.path()).await;
	fs::read(tmp_file.path())
		.await
		.map_err(|_| ApiError::ImageProcessingError)
}
