/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import org.apache.geode.CancelException;
import org.apache.geode.CopyException;
import org.apache.geode.GemFireCacheException;
import org.apache.geode.GemFireCheckedException;
import org.apache.geode.GemFireConfigException;
import org.apache.geode.GemFireException;
import org.apache.geode.GemFireIOException;
import org.apache.geode.IncompatibleSystemException;
import org.apache.geode.InternalGemFireException;
import org.apache.geode.InvalidValueException;
import org.apache.geode.LicenseException;
import org.apache.geode.NoSystemException;
import org.apache.geode.SystemConnectException;
import org.apache.geode.SystemIsRunningException;
import org.apache.geode.UnmodifiableException;
import org.apache.geode.cache.CacheException;
import org.apache.geode.cache.CacheExistsException;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.CacheRuntimeException;
import org.apache.geode.cache.CacheWriterException;
import org.apache.geode.cache.CacheXmlException;
import org.apache.geode.cache.CommitConflictException;
import org.apache.geode.cache.CommitIncompleteException;
import org.apache.geode.cache.DiskAccessException;
import org.apache.geode.cache.EntryDestroyedException;
import org.apache.geode.cache.EntryExistsException;
import org.apache.geode.cache.EntryNotFoundException;
import org.apache.geode.cache.FailedSynchronizationException;
import org.apache.geode.cache.OperationAbortedException;
import org.apache.geode.cache.PartitionedRegionDistributionException;
import org.apache.geode.cache.PartitionedRegionStorageException;
import org.apache.geode.cache.RegionDestroyedException;
import org.apache.geode.cache.RegionExistsException;
import org.apache.geode.cache.ResourceException;
import org.apache.geode.cache.RoleException;
import org.apache.geode.cache.StatisticsDisabledException;
import org.apache.geode.cache.SynchronizationCommitConflictException;
import org.apache.geode.cache.VersionException;
import org.apache.geode.cache.client.ServerConnectivityException;
import org.apache.geode.cache.execute.FunctionException;
import org.apache.geode.cache.query.CqClosedException;
import org.apache.geode.cache.query.IndexInvalidException;
import org.apache.geode.cache.query.IndexMaintenanceException;
import org.apache.geode.cache.query.QueryException;
import org.apache.geode.cache.query.QueryExecutionTimeoutException;
import org.apache.geode.cache.query.QueryInvalidException;
import org.apache.geode.distributed.LeaseExpiredException;
import org.apache.geode.security.GemFireSecurityException;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.dao.InvalidDataAccessResourceUsageException;
import org.springframework.dao.PermissionDeniedDataAccessException;
import org.springframework.dao.PessimisticLockingFailureException;
import org.springframework.dao.TypeMismatchDataAccessException;
import org.springframework.util.ClassUtils;

/**
 * Helper class featuring methods for GemFire Cache or Region handling.
 *
 * @author Costin Leau
 */
public abstract class GemfireCacheUtils {

	private static Class<?> CQ_EXCEPTION_CLASS;

	static {
		Class<?> type = null;

		try {
			type = ClassUtils.resolveClassName("org.apache.geode.cache.query.CqInvalidException",
				GemfireCacheUtils.class.getClassLoader());

		}
		catch (IllegalArgumentException ignore) {
		}

		CQ_EXCEPTION_CLASS = type;
	}


	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy.
	 *
	 * @param ex Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	@SuppressWarnings("deprecation")
	public static DataAccessException convertGemfireAccessException(GemFireException ex) {
		if (ex instanceof CacheException) {
			if (ex instanceof CacheExistsException) {
				return new DataIntegrityViolationException(ex.getMessage(), ex);
			}
			if (ex instanceof CommitConflictException) {
				return new DataIntegrityViolationException(ex.getMessage(), ex);
			}
			if (ex instanceof CommitIncompleteException) {
				return new DataIntegrityViolationException(ex.getMessage(), ex);
			}
			if (ex instanceof EntryExistsException) {
				return new DuplicateKeyException(ex.getMessage(), ex);
			}
			if (ex instanceof EntryNotFoundException) {
				return new DataRetrievalFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof RegionExistsException) {
				return new DataIntegrityViolationException(ex.getMessage(), ex);
			}
		}
		if (ex instanceof CacheRuntimeException) {
			if (ex instanceof CacheXmlException) {
				return new GemfireSystemException(ex);
			}
			if (ex instanceof CancelException) {
				// all cancellations go wrapped by this exception
				return new GemfireCancellationException((CancelException) ex);
			}
			if (ex instanceof CqClosedException) {
				return new InvalidDataAccessApiUsageException(ex.getMessage(), ex);
			}
			if (ex instanceof DiskAccessException) {
				return new DataAccessResourceFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof EntryDestroyedException) {
				return new InvalidDataAccessApiUsageException(ex.getMessage(), ex);
			}
			if (ex instanceof FailedSynchronizationException) {
				return new PessimisticLockingFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof IndexMaintenanceException) {
				return new GemfireIndexException((IndexMaintenanceException) ex);
			}
			if (ex instanceof OperationAbortedException) {
				// treat user exceptions first
				if (ex instanceof CacheLoaderException) {
					return new GemfireSystemException(ex);
				}
				if (ex instanceof CacheWriterException) {
					return new GemfireSystemException(ex);
				}
				// the rest are treated as resource failures
				return new DataAccessResourceFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof PartitionedRegionDistributionException) {
				return new DataAccessResourceFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof PartitionedRegionStorageException) {
				return new DataAccessResourceFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof QueryExecutionTimeoutException) {
				return new GemfireQueryException((QueryExecutionTimeoutException) ex);
			}
			if (ex instanceof RegionDestroyedException) {
				return new InvalidDataAccessResourceUsageException(ex.getMessage(), ex);
			}
			if (ex instanceof org.apache.geode.admin.RegionNotFoundException) {
				return new InvalidDataAccessResourceUsageException(ex.getMessage(), ex);
			}
			if (ex instanceof ResourceException) {
				return new DataAccessResourceFailureException(ex.getMessage(), ex);
			}
			if (ex instanceof RoleException) {
				return new GemfireSystemException(ex);
			}
			if (ex instanceof StatisticsDisabledException) {
				return new GemfireSystemException(ex);
			}
			if (ex instanceof SynchronizationCommitConflictException) {
				return new PessimisticLockingFailureException(ex.getMessage(), ex);
			}
		}
		if (ex instanceof CopyException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof org.apache.geode.cache.EntryNotFoundInRegion) {
			return new DataRetrievalFailureException(ex.getMessage(), ex);
		}
		if (ex instanceof FunctionException) {
			return new InvalidDataAccessApiUsageException(ex.getMessage(), ex);
		}
		if (ex instanceof GemFireCacheException) {
			return convertGemfireAccessException(((GemFireCacheException) ex).getCacheException());
		}
		if (ex instanceof GemFireConfigException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof GemFireIOException) {
			return new DataAccessResourceFailureException(ex.getMessage(), ex);
		}
		if (ex instanceof GemFireSecurityException) {
			return new PermissionDeniedDataAccessException(ex.getMessage(), ex);
		}
		if (ex instanceof IncompatibleSystemException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof InternalGemFireException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof InvalidValueException) {
			return new TypeMismatchDataAccessException(ex.getMessage(), ex);
		}
		if (ex instanceof LeaseExpiredException) {
			return new PessimisticLockingFailureException(ex.getMessage(), ex);
		}
		if (ex instanceof LicenseException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof NoSystemException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof org.apache.geode.admin.RuntimeAdminException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof ServerConnectivityException) {
			return new DataAccessResourceFailureException(ex.getMessage(), ex);
		}
		if (ex instanceof SystemConnectException) {
			return new DataAccessResourceFailureException(ex.getMessage(), ex);
		}
		if (ex instanceof SystemIsRunningException) {
			return new GemfireSystemException(ex);
		}
		if (ex instanceof UnmodifiableException) {
			return new GemfireSystemException(ex);
		}

		// for exceptions that had their parent changed in 6.5
		DataAccessException dae = convertQueryExceptions(ex);
		if (dae != null)
			return dae;

		// fall back
		return new GemfireSystemException(ex);
	}

	/**
	 * Dedicated method for converting exceptions changed in 6.5 that had their
	 * parent changed. This method exists to 'fool' the compiler type checks
	 * by loosening the type so the code compiles on both 6.5 (pre and current) branches.
	 */
	static DataAccessException convertQueryExceptions(RuntimeException ex) {
		if (ex instanceof IndexInvalidException) {
			return convertGemfireAccessException((IndexInvalidException) ex);
		}
		if (ex instanceof QueryInvalidException) {
			return convertGemfireAccessException((QueryInvalidException) ex);
		}

		if (isCqInvalidException(ex)) {
			return convertCqInvalidException(ex);
		}

		// fall back
		return new GemfireSystemException(ex);
	}

	/**
	 * Converts the given (checked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy.
	 *
	 * @param ex Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	@SuppressWarnings("deprecation")
	public static DataAccessException convertGemfireAccessException(GemFireCheckedException ex) {
		// query exceptions
		if (ex instanceof QueryException) {
			return new GemfireQueryException((QueryException) ex);
		}
		// version exception
		if (ex instanceof VersionException) {
			return new DataAccessResourceFailureException(ex.getMessage(), ex);
		}
		// admin exception
		if (ex instanceof org.apache.geode.admin.AdminException) {
			return new GemfireSystemException(ex);
		}
		// fall back
		return new GemfireSystemException(ex);
	}

	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy. This method exists to handle backwards compatibility
	 * for exceptions that had their parents changed in GemFire 6.5.
	 *
	 * @param ex Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	public static DataAccessException convertGemfireAccessException(IndexInvalidException ex) {
		return new GemfireIndexException(ex);
	}

	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy. This method exists to handle backwards compatibility
	 * for exceptions that had their parents changed in GemFire 6.5.
	 *
	 * @param ex Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	public static DataAccessException convertGemfireAccessException(QueryInvalidException ex) {
		return new GemfireQueryException(ex);
	}

	/**
	 * Package protected method for detecting CqInvalidException which has been removed in GemFire 6.5 GA.
	 */
	static boolean isCqInvalidException(RuntimeException ex) {
		return (CQ_EXCEPTION_CLASS != null && CQ_EXCEPTION_CLASS.isAssignableFrom(ex.getClass()));
	}

	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy. This method exists to handle backwards compatibility
	 * for exceptions that have been removed in 6.5.
	 *
	 * @param ex Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	static DataAccessException convertCqInvalidException(RuntimeException ex) {
		return new GemfireQueryException(ex);
	}

}
