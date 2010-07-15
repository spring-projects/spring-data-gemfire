/*
 * Copyright 2010 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.dao.DataAccessException;

import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;

/**
 * Base class for GemfireTemplate and GemfireInterceptor, defining common properties such as {@link Region}. 
 * <p/>
 * Not intended to be used directly.
 * 
 * @author Costin Leau
 */
public class GemfireAccessor implements InitializingBean {

	/** Logger available to subclasses */
	protected final Log log = LogFactory.getLog(getClass());

	private Region<?, ?> region;

	public void afterPropertiesSet() {
		if (getRegion() == null) {
			throw new IllegalArgumentException("Property 'region' is required");
		}
	}

	/**
	 * Converts the given {@link GemFireCheckedException} to an appropriate exception from the
	 * <code>org.springframework.dao</code> hierarchy.
	 * May be overridden in subclasses.
	 * @param ex GemFireCheckedException that occurred
	 * @return the corresponding DataAccessException instance
	 */
	public DataAccessException convertGemFireAccessException(GemFireCheckedException ex) {
		return GemfireCacheUtils.convertGemfireAccessException(ex);
	}

	/**
	 * Converts the given {@link GemFireException} to an appropriate exception from the
	 * <code>org.springframework.dao</code> hierarchy.
	 * May be overridden in subclasses.
	 * @param ex GemFireException that occurred
	 * @return the corresponding DataAccessException instance
	 */
	public DataAccessException convertGemFireAccessException(GemFireException ex) {
		return GemfireCacheUtils.convertGemfireAccessException(ex);
	}

	/**
	 * Returns the template region.
	 * 
	 * @return the region
	 */
	public Region<?, ?> getRegion() {
		return region;
	}

	/**
	 * Sets the template region.
	 * 
	 * @param region the region to set
	 */
	public void setRegion(Region<?, ?> region) {
		this.region = region;
	}
}