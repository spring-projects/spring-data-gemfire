/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType.IDLE_TIMEOUT;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.gemstone.gemfire.cache.Region;

import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.ExpirationActionType;
import org.springframework.data.gemfire.support.Expiration;
import org.springframework.data.gemfire.support.IdleTimeoutExpiration;
import org.springframework.data.gemfire.support.TimeToLiveExpiration;

/**
 * The {@link EnableExpiration} annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to enable {@link Region} entry expiration for individual entries.  Note, this annotation does not
 * cover {@link Region} expiration; {@link Region} expiration must be configure on the {@link Region} definition itself.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration
 * @see org.springframework.data.gemfire.support.Expiration
 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
 * @see com.gemstone.gemfire.cache.Region
 * @see <a href="http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/#bootstrap:region:expiration:annotation">Annotation-based Data Expiration</a>
 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/developing/expiration/chapter_overview.html">GemFire Expiration</a>
 * @see <a href="http://geode.incubator.apache.org/docs/guide/developing/expiration/chapter_overview.html">Geode Expiration</a>
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(ExpirationConfiguration.class)
@SuppressWarnings({ "unused" })
public @interface EnableExpiration {

	/**
	 * Defines individual {@link Region} Expiration policies or customizes the default Expiration policy
	 * for all {@link Region Regions}.
	 *
	 * Defaults to empty.
	 */
	ExpirationPolicy[] policies() default {};

	/**
	 * Definition for a specific Expiration policy that can be applied to 1 or more {@link Region Regions}.
	 *
	 * An Expiration policy defines the expiration timeout and expiration action to take when
	 * an {@link Region} entry times out.
	 *
	 * Additionally, the Expiration policy defines the algorithm to use (e.g. Idle Timeout (TTI) or Time-To-Live (TTL),
	 * or both) to determine if and when an {@link Region} entry has timed out.
	 */
	@interface ExpirationPolicy {

		/**
		 * Specifies the timeout used to determine when a {@link Region} entry expires.
		 *
		 * This value of this attribute determines the "default" timeout used if no specific timeout was specified.
		 * A specific timeout is determined by {@link Expiration#timeout()}, {@link IdleTimeoutExpiration#timeout()}
		 * or {@link TimeToLiveExpiration#timeout()} attribute on the application domain object.
		 *
		 * See the SDG Reference Guide for more details...
		 *
		 * @see <a href="http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/#bootstrap:region:expiration:annotation">Annotation-based Data Expiration</a>
		 */
		int timeout();

		/**
		 * Specifies the action taken when a {@link Region} entry expires.
		 *
		 * This value of this attribute determines the "default" action taken if no specific action was specified.
		 * The specific action is determined by {@link Expiration#action()}, {@link IdleTimeoutExpiration#action()}
		 * or {@link TimeToLiveExpiration#action()} attribute on the application domain object.
		 *
		 * See the SDG Reference Guide for more details...
		 *
		 * @see <a href="http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/#bootstrap:region:expiration:annotation">Annotation-based Data Expiration</a>
		 */
		ExpirationActionType action();

		/**
		 * Names of specific {@link Region Regions} on which this Expiration policy is applied.
		 *
		 * If no {@link Region} names are specified then this Expiration policy will apply to
		 * all {@link Region Regions} declared in the Spring context.
		 *
		 * Defaults to all {@link Region Regions}.
		 */
		String[] regionNames() default {};

		/**
		 * Types of Expiration algorithms (Idle Timeout (TTI) or Time to Live (TTL)) configured and used by
		 * {@link Region Region(s)} to expire entries.
		 *
		 * Defaults to both Idle Timeout (TTI).
		 *
		 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType
		 */
		ExpirationType[] types() default { IDLE_TIMEOUT };

	}

	/**
	 * {@link ExpirationType} defines different types of GemFire/Geode Expiration policies such as
	 * (Entry) Idle Timeout (TTI) and (Entry) Time to Live (TTL).
	 *
	 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/developing/expiration/chapter_overview.html">GemFire Expiration</a>
	 * @see <a href="http://geode.incubator.apache.org/docs/guide/developing/expiration/chapter_overview.html">Geode Expiration</a>
	 */
	enum ExpirationType {
		IDLE_TIMEOUT("TTI"),
		TIME_TO_LIVE("TTL");

		private final String abbreviation;

		/**
		 * Factory method to lookup an appropriate {@link ExpirationType} based on an abbreviation.
		 *
		 * @param abbreviation abbreviation used to lookup the appropriate {@link ExpirationType}.
		 * @return an {@link ExpirationType} matching the abbreviation or {@literal null} if the abbreviation
		 * does not match an {@link ExpirationType}.
		 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType
		 * @see #values()
		 * @see #abbreviation()
		 */
		static ExpirationType valueOfAbbreviation(String abbreviation) {
			for (ExpirationType expirationType : values()) {
				if (expirationType.abbreviation().equalsIgnoreCase(abbreviation)) {
					return expirationType;
				}
			}

			return null;
		}

		/**
		 * Constructs a new instance of {@link ExpirationType} initialized with the given abbreviation.
		 *
		 * @param abbreviation {@link String} indicating the {@link ExpirationType} abbreviation.
		 */
		ExpirationType(String abbreviation) {
			this.abbreviation = abbreviation;
		}

		/**
		 * Returns the abbreviation for this {@link ExpirationType}.
		 *
		 * @return a {@link String} with the {@link ExpirationType} abbreviation.
		 */
		protected String abbreviation() {
			return this.abbreviation;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public String toString() {
			return String.format("%1$s (%2$s)", name(), abbreviation());
		}
	}
}
