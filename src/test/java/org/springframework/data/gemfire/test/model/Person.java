/*
 * Copyright 2012-2018 the original author or authors.
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

package org.springframework.data.gemfire.test.model;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceConstructor;
import org.springframework.data.gemfire.mapping.annotation.Indexed;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.test.support.IdentifierSequence;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * The {@link Person} class is an Abstract Data Type (ADT) modeling a person.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.util.Calendar
 * @see java.util.Date
 * @see org.springframework.data.annotation.Id
 * @see org.springframework.data.annotation.PersistenceConstructor
 * @see org.springframework.data.gemfire.mapping.annotation.Region
 * @see org.springframework.data.gemfire.mapping.annotation.Indexed
 * @see org.springframework.data.gemfire.test.support.IdentifierSequence
 * @since 2.0.0
 */
@Region("People")
@SuppressWarnings("unused")
public class Person implements Comparable<Person>, Serializable {

	protected static final String PERSON_TO_STRING =
		"{ @type = %1$s, id = %2$d, firstName = %3$s, lastName = %4$s, birthDate = %5$s, gender = %6$s}";

	protected static final String BIRTH_DATE_PATTERN = "yyyy/MM/dd";

	private Date birthDate;

	private Gender gender;

	@Id
	private Long id;

	private final String firstName;

	@Indexed
	private final String lastName;

	public static Date newBirthDate(int year, int month, int dayOfMonth) {

		Calendar birthDate = Calendar.getInstance();

		birthDate.clear();
		birthDate.set(Calendar.YEAR, year);
		birthDate.set(Calendar.MONTH, month);
		birthDate.set(Calendar.DAY_OF_MONTH, dayOfMonth);

		return birthDate.getTime();
	}

	public static Person newPerson(String firstName, String lastName, Date birthDate, Gender gender) {
		return newPerson(IdentifierSequence.nextId(), firstName, lastName, birthDate, gender);
	}

	public static Person newPerson(Long id, String firstName, String lastName, Date birthDate, Gender gender) {
		return new Person(id, firstName, lastName, birthDate, gender);
	}

	@PersistenceConstructor
	public Person(String firstName, String lastName, Date birthDate, Gender gender) {

		Assert.hasText(firstName, "firstName is required");
		Assert.hasText(lastName, "lastName is required");

		this.firstName = firstName;
		this.lastName = lastName;
		this.birthDate = (birthDate != null ? (Date) birthDate.clone() : null);
		this.gender = gender;
	}

	public Person(Long id, String firstName, String lastName, Date birthDate, Gender gender) {
		this(firstName, lastName, birthDate, gender);
		this.id = id;
	}

	public Long getId() {
		return this.id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Date getBirthDate() {
		return this.birthDate;
	}

	public void setBirthDate(Date birthDate) {
		this.birthDate = (birthDate != null ? (Date) birthDate.clone() : null);
	}

	public String getFirstName() {
		return this.firstName;
	}

	public Gender getGender() {
		return this.gender;
	}

	public void setGender(Gender gender) {
		this.gender = gender;
	}

	public String getLastName() {
		return this.lastName;
	}

	public String getName() {
		return String.format("%1$s %2$s", getFirstName(), getLastName());
	}

	@Override
	public int compareTo(Person that) {

		int result = nullSafeCompareTo(this.getLastName(), that.getLastName());

		result = result != 0 ? result : nullSafeCompareTo(this.getFirstName(), that.getLastName());
		result = result != 0 ? result : nullSafeCompareTo(this.getBirthDate(), that.getBirthDate());

		return result;
	}

	private <T extends Comparable<T>> int nullSafeCompareTo(T comparableOne, T comparableTwo) {

		return comparableOne == null ? 1
			: comparableTwo == null ? -1
			: comparableOne.compareTo(comparableTwo);
	}

	@Override
	public boolean equals(Object obj) {

		if (this == obj) {
			return true;
		}

		if (!(obj instanceof Person)) {
			return false;
		}

		Person that = (Person) obj;

		return SpringUtils.equalsIgnoreNull(this.getId(), that.getId())
			&& (ObjectUtils.nullSafeEquals(this.getBirthDate(), that.getBirthDate()))
			&& (ObjectUtils.nullSafeEquals(this.getFirstName(), that.getFirstName())
			&& (ObjectUtils.nullSafeEquals(this.getGender(), that.getGender()))
			&& (ObjectUtils.nullSafeEquals(this.getLastName(), that.getLastName())));
	}

	@Override
	public int hashCode() {

		int hashValue = 17;

		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getId());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getBirthDate());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getFirstName());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getLastName());

		return hashValue;
	}

	@Override
	public String toString() {
		return String.format(PERSON_TO_STRING, getClass().getName(), getId(), getFirstName(), getLastName(),
			toString(getBirthDate(), BIRTH_DATE_PATTERN), getGender());
	}

	protected static String toString(Date dateTime, String DATE_FORMAT_PATTERN) {

		return dateTime != null
			? new SimpleDateFormat(DATE_FORMAT_PATTERN).format(dateTime)
			: null;
	}
}
