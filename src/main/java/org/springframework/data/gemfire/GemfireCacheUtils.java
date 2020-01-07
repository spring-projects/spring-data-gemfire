/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
import org.apache.geode.NoSystemException;
import org.apache.geode.SystemConnectException;
import org.apache.geode.SystemIsRunningException;
import org.apache.geode.UnmodifiableException;
import org.apache.geode.admin.AdminException;
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
 * Abstract utility class featuring methods for Apache Geode / Pivotal GemFire Cache or Region handling.
 *
 * @author Costin Leau
 * @author John Blum
 */
public abstract class GemfireCacheUtils {

	private static Class<?> CQ_EXCEPTION_CLASS;

	static {

		Class<?> type = null;

		try {
			type = ClassUtils.resolveClassName("org.apache.geode.cache.query.CqInvalidException",
				GemfireCacheUtils.class.getClassLoader());
		}
		catch (IllegalArgumentException ignore) { }

		CQ_EXCEPTION_CLASS = type;
	}


	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy.
	 *
	 * @param cause Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	@SuppressWarnings("deprecation")
	public static DataAccessException convertGemfireAccessException(GemFireException cause) {

		if (cause instanceof CacheException) {
			if (cause instanceof CacheExistsException) {
				return new DataIntegrityViolationException(cause.getMessage(), cause);
			}
			if (cause instanceof CommitConflictException) {
				return new DataIntegrityViolationException(cause.getMessage(), cause);
			}
			if (cause instanceof CommitIncompleteException) {
				return new DataIntegrityViolationException(cause.getMessage(), cause);
			}
			if (cause instanceof EntryExistsException) {
				return new DuplicateKeyException(cause.getMessage(), cause);
			}
			if (cause instanceof EntryNotFoundException) {
				return new DataRetrievalFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof RegionExistsException) {
				return new DataIntegrityViolationException(cause.getMessage(), cause);
			}
		}

		if (cause instanceof CacheRuntimeException) {
			if (cause instanceof CacheXmlException) {
				return new GemfireSystemException(cause);
			}
			if (cause instanceof CancelException) {
				// all cancellations go wrapped by this exception
				return new GemfireCancellationException((CancelException) cause);
			}
			if (cause instanceof CqClosedException) {
				return new InvalidDataAccessApiUsageException(cause.getMessage(), cause);
			}
			if (cause instanceof DiskAccessException) {
				return new DataAccessResourceFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof EntryDestroyedException) {
				return new InvalidDataAccessApiUsageException(cause.getMessage(), cause);
			}
			if (cause instanceof FailedSynchronizationException) {
				return new PessimisticLockingFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof IndexMaintenanceException) {
				return new GemfireIndexException((IndexMaintenanceException) cause);
			}
			if (cause instanceof OperationAbortedException) {
				// treat user exceptions first
				if (cause instanceof CacheLoaderException) {
					return new GemfireSystemException(cause);
				}
				if (cause instanceof CacheWriterException) {
					return new GemfireSystemException(cause);
				}
				// the rest are treated as resource failures
				return new DataAccessResourceFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof PartitionedRegionDistributionException) {
				return new DataAccessResourceFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof PartitionedRegionStorageException) {
				return new DataAccessResourceFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof QueryExecutionTimeoutException) {
				return new GemfireQueryException((QueryExecutionTimeoutException) cause);
			}
			if (cause instanceof RegionDestroyedException) {
				return new InvalidDataAccessResourceUsageException(cause.getMessage(), cause);
			}
			if (cause instanceof org.apache.geode.admin.RegionNotFoundException) {
				return new InvalidDataAccessResourceUsageException(cause.getMessage(), cause);
			}
			if (cause instanceof ResourceException) {
				return new DataAccessResourceFailureException(cause.getMessage(), cause);
			}
			if (cause instanceof RoleException) {
				return new GemfireSystemException(cause);
			}
			if (cause instanceof StatisticsDisabledException) {
				return new GemfireSystemException(cause);
			}
			if (cause instanceof SynchronizationCommitConflictException) {
				return new PessimisticLockingFailureException(cause.getMessage(), cause);
			}
		}

		if (cause instanceof CopyException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof FunctionException) {
			return new InvalidDataAccessApiUsageException(cause.getMessage(), cause);
		}
		if (cause instanceof GemFireCacheException) {
			return convertGemfireAccessException(((GemFireCacheException) cause).getCacheException());
		}
		if (cause instanceof GemFireConfigException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof GemFireIOException) {
			return new DataAccessResourceFailureException(cause.getMessage(), cause);
		}
		if (cause instanceof GemFireSecurityException) {
			return new PermissionDeniedDataAccessException(cause.getMessage(), cause);
		}
		if (cause instanceof IncompatibleSystemException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof InternalGemFireException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof InvalidValueException) {
			return new TypeMismatchDataAccessException(cause.getMessage(), cause);
		}
		if (cause instanceof LeaseExpiredException) {
			return new PessimisticLockingFailureException(cause.getMessage(), cause);
		}
		if (cause instanceof NoSystemException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof org.apache.geode.admin.RuntimeAdminException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof ServerConnectivityException) {
			return new DataAccessResourceFailureException(cause.getMessage(), cause);
		}
		if (cause instanceof SystemConnectException) {
			return new DataAccessResourceFailureException(cause.getMessage(), cause);
		}
		if (cause instanceof SystemIsRunningException) {
			return new GemfireSystemException(cause);
		}
		if (cause instanceof UnmodifiableException) {
			return new GemfireSystemException(cause);
		}

		// for exceptions that had their parent changed in 6.5
		DataAccessException dataAccessException = convertQueryExceptions(cause);

		if (dataAccessException != null) {
			return dataAccessException;
		}

		return new GemfireSystemException(cause);
	}

	/**
	 * Converts the given (checked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy.
	 *
	 * @param cause Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	@SuppressWarnings("deprecation")
	public static DataAccessException convertGemfireAccessException(GemFireCheckedException cause) {

		if (cause instanceof AdminException) {
			return new GemfireSystemException(cause);
		}

		if (cause instanceof QueryException) {
			return new GemfireQueryException((QueryException) cause);
		}

		if (cause instanceof VersionException) {
			return new DataAccessResourceFailureException(cause.getMessage(), cause);
		}

		return new GemfireSystemException(cause);
	}

	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy. This method exists to handle backwards compatibility
	 * for exceptions that had their parents changed in Pivotal GemFire 6.5.
	 *
	 * @param cause Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	public static DataAccessException convertGemfireAccessException(IndexInvalidException cause) {
		return new GemfireIndexException(cause);
	}

	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy. This method exists to handle backwards compatibility
	 * for exceptions that had their parents changed in Pivotal GemFire 6.5.
	 *
	 * @param cause Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	public static DataAccessException convertGemfireAccessException(QueryInvalidException cause) {
		return new GemfireQueryException(cause);
	}

	/**
	 * Package protected method for detecting CqInvalidException which has been removed in Pivotal GemFire 6.5 GA.
	 */
	static boolean isCqInvalidException(RuntimeException cause) {
		return CQ_EXCEPTION_CLASS != null && CQ_EXCEPTION_CLASS.isInstance(cause);
	}

	/**
	 * Dedicated method for converting exceptions changed in 6.5 that had their
	 * parent changed. This method exists to 'fool' the compiler type checks
	 * by loosening the type so the code compiles on both 6.5 (pre and current) branches.
	 */
	static DataAccessException convertQueryExceptions(RuntimeException cause) {

		if (cause instanceof IndexInvalidException) {
			return convertGemfireAccessException((IndexInvalidException) cause);
		}

		if (cause instanceof QueryInvalidException) {
			return convertGemfireAccessException((QueryInvalidException) cause);
		}

		if (isCqInvalidException(cause)) {
			return convertCqInvalidException(cause);
		}

		return new GemfireSystemException(cause);
	}

	/**
	 * Converts the given (unchecked) Gemfire exception to an appropriate one from the
	 * <code>org.springframework.dao</code> hierarchy. This method exists to handle backwards compatibility
	 * for exceptions that have been removed in 6.5.
	 *
	 * @param cause Gemfire unchecked exception
	 * @return new the corresponding DataAccessException instance
	 */
	static DataAccessException convertCqInvalidException(RuntimeException cause) {
		return new GemfireQueryException(cause);
	}
}
