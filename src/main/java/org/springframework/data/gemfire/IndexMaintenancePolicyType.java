/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * The IndexMaintenanceType enum is a enumerated type of GemFire Index maintenance update options.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.AttributesFactory#setIndexMaintenanceSynchronous(boolean)
 * @see com.gemstone.gemfire.cache.RegionAttributes#getIndexMaintenanceSynchronous()
 * @see com.gemstone.gemfire.cache.RegionFactory#setIndexMaintenanceSynchronous(boolean)
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public enum IndexMaintenancePolicyType {
	SYNCHRONOUS,
	ASYNCHRONOUS;

	public static final IndexMaintenancePolicyType DEFAULT = IndexMaintenancePolicyType.SYNCHRONOUS;

	/**
	 * Return an IndexMaintenanceType enumerated value given a case-insensitive, named String value
	 * describing the type of Index maintenance.
	 *
	 * @param name the String value indicating the type of Index maintenance (update).
	 * @return an IndexMaintenanceType enumerated value given a case-insensitive, named String value describing
	 * the type of Index maintenance, or null if no match was found.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #name()
	 */
	public static IndexMaintenancePolicyType valueOfIgnoreCase(final String name) {
		for (IndexMaintenancePolicyType indexMaintenancePolicyType : values()) {
			if (indexMaintenancePolicyType.name().equalsIgnoreCase(name)) {
				return indexMaintenancePolicyType;
			}
		}

		return null;
	}

	/**
	 * Sets the GemFire AttributesFactory's 'indexMaintenanceSynchronous' property appropriately based on
	 * this IndexMaintenancePolicyType.
	 *
	 * @param attributesFactory the AttributesFactory instance on which to set the indexMaintenanceProperty.
	 * @throws java.lang.NullPointerException if the AttributesFactory reference is null.
	 * @see #setIndexMaintenance(com.gemstone.gemfire.cache.RegionFactory)
	 */
	@SuppressWarnings("deprecation")
	public void setIndexMaintenance(final AttributesFactory attributesFactory) {
		attributesFactory.setIndexMaintenanceSynchronous(equals(SYNCHRONOUS));
	}

	/**
	 * Sets the GemFire RegionFactory's 'indexMaintenanceSynchronous' property appropriately based on
	 * this IndexMaintenancePolicyType.
	 *
	 * @param regionFactory the RegionFactory instance on which to set the indexMaintenanceProperty.
	 * @throws java.lang.NullPointerException if the RegionFactory reference is null.
	 * @see #setIndexMaintenance(com.gemstone.gemfire.cache.AttributesFactory)
	 */
	public void setIndexMaintenance(final RegionFactory regionFactory) {
		regionFactory.setIndexMaintenanceSynchronous(equals(SYNCHRONOUS));
	}

}
